#!/usr/bin/env python3
"""
Remove GMP (WITH_GMP) conditionals from src/s7.c incrementally.

Strategy:
1. Preprocessor blocks: use a stack-based parser to find #if blocks.
   - #if WITH_GMP / #ifdef WITH_GMP -> delete entire block
   - #if !WITH_GMP / #ifndef WITH_GMP -> unwrap (remove #if/#endif, keep body)
2. Runtime expressions: regex replacements for ternary/boolean uses.
"""

import argparse
import re
import sys
from pathlib import Path


def preprocess_condition(cond):
    """
    Given a condition string from an #if directive, replace WITH_GMP with 0
    and simplify the expression as much as possible.
    """
    # Replace WITH_GMP with 0
    cond = re.sub(r'\bWITH_GMP\b', '0', cond)
    # Basic constant folding for 0/1 with && and ||
    # This is not a full C preprocessor evaluator, but covers the patterns in s7.c
    changed = True
    while changed:
        changed = False
        # 0 && x -> 0
        if re.search(r'\b0\s*&&', cond):
            cond = re.sub(r'0\s*&&\s*[^)]+', '0', cond)
            changed = True
        # 1 && x -> x (but we don't have 1 here typically)
        # 0 || x -> x
        if re.search(r'\b0\s*\|\|', cond):
            cond = re.sub(r'0\s*\|\|\s*', '', cond)
            changed = True
        # !0 -> 1
        if '!0' in cond:
            cond = cond.replace('!0', '1')
            changed = True
    return cond.strip()


def find_preproc_blocks(lines):
    """
    Parse all preprocessor blocks in lines.
    Returns list of dicts with keys: start, end, kind, cond, depth, lines_range.
    """
    stack = []
    blocks = []
    for i, line in enumerate(lines):
        stripped = line.lstrip()
        if stripped.startswith('#if '):
            cond = stripped[3:].strip()
            stack.append({
                'start': i,
                'kind': 'if',
                'cond': cond,
                'depth': len(stack),
                'has_else': False,
                'else_line': None
            })
        elif stripped.startswith('#ifdef '):
            cond = stripped[7:].strip()
            stack.append({
                'start': i,
                'kind': 'ifdef',
                'cond': cond,
                'depth': len(stack),
                'has_else': False,
                'else_line': None
            })
        elif stripped.startswith('#ifndef '):
            cond = stripped[8:].strip()
            stack.append({
                'start': i,
                'kind': 'ifndef',
                'cond': cond,
                'depth': len(stack),
                'has_else': False,
                'else_line': None
            })
        elif stripped.startswith('#elif'):
            if stack:
                stack[-1]['kind'] = 'elif'
        elif stripped.startswith('#else'):
            if stack:
                stack[-1]['has_else'] = True
                stack[-1]['else_line'] = i
        elif stripped.startswith('#endif'):
            if stack:
                block = stack.pop()
                block['end'] = i
                blocks.append(block)
    return blocks


def classify_block(block):
    """
    Classify a block as:
    - 'delete': entire block should be removed (condition is always false)
    - 'unwrap': #if/#endif wrapper should be removed, body kept (condition is always true)
    - 'keep': block should be left as-is (condition still depends on something else)
    - 'none': not a WITH_GMP block
    """
    cond = block['cond']
    kind = block['kind']
    
    if 'WITH_GMP' not in cond:
        return 'none'
    
    # Normalize condition
    if kind == 'ifdef':
        cond = f'defined({cond})'
    elif kind == 'ifndef':
        cond = f'!defined({cond})'
    
    simplified = preprocess_condition(cond)
    
    if simplified == '0':
        return 'delete'
    elif simplified == '1':
        return 'unwrap'
    else:
        # Still has unresolved dependencies, but WITH_GMP has been folded.
        # We should replace the condition with the simplified one.
        return 'replace'


def remove_preproc_blocks(lines, blocks_to_process):
    """
    Given original lines and a list of blocks, apply deletions/unwraps.
    blocks_to_process must be sorted by end line descending.
    """
    lines = list(lines)
    
    for block in blocks_to_process:
        start = block['start']
        end = block['end']
        action = block['action']
        kind = block['kind']
        has_else = block.get('has_else', False)
        else_line = block.get('else_line', None)
        simplified = block.get('simplified', '')
        
        if action == 'delete':
            # Remove entire block from start to end (inclusive)
            # But if there's an #else, we need to be careful.
            # For #if 0 blocks, the #else branch is the active one.
            # However, in our case, all WITH_GMP blocks are simple:
            # either #if WITH_GMP (positive) -> delete everything
            # or #if !WITH_GMP (negative) -> unwrap, keep everything
            # So for 'delete', there's no #else that we want to keep.
            for i in range(start, end + 1):
                lines[i] = None
        elif action == 'unwrap':
            # Remove #if line and #endif line, keep everything in between
            # If there's an #else, since condition is always true, the else branch is dead.
            # But our blocks shouldn't have #else if they're unwrap (always true).
            lines[start] = None
            lines[end] = None
            if has_else:
                # Remove #else and everything after it within the block
                for i in range(else_line, end):
                    lines[i] = None
        elif action == 'replace':
            # Replace the #if condition with simplified one
            old_line = lines[start]
            # Replace the condition part after #if
            if kind in ('ifdef', 'ifndef'):
                new_line = f'#if {simplified}\n'
            else:
                new_line = re.sub(r'(#if\s+).*', r'\1' + simplified, old_line)
                if not new_line.endswith('\n'):
                    new_line += '\n'
            lines[start] = new_line
    
    return [l for l in lines if l is not None]


def fix_runtime_expressions(lines):
    """
    Fix runtime uses of WITH_GMP (not in preprocessor directives).
    Patterns found in s7.c:
    - (WITH_GMP) ? A : B  ->  B
    - (!WITH_GMP) ? A : B  ->  A
    - if ((!WITH_GMP) && (cond)) stmt  ->  if (cond) stmt
    - if (WITH_GMP) return(false);  -> remove line
    - ((!WITH_GMP) && (cond)) ? A : B  ->  (cond) ? A : B
    - (WITH_GMP) ? sc->is_list_symbol : sc->is_pair_symbol  -> sc->is_pair_symbol
    """
    new_lines = []
    in_block_comment = False
    for line in lines:
        original = line
        
        # Track block comments to avoid replacing WITH_GMP inside comments
        stripped = line.lstrip()
        if not in_block_comment:
            # Check for /* ... */ on same line, or /* starting
            if '/*' in line:
                before = line.split('/*', 1)[0]
                after = line.split('/*', 1)[1]
                if '*/' not in after:
                    in_block_comment = True
                # Only process the part before /*
                # For simplicity, if line has /* we skip processing the whole line
                # This is conservative but safe for s7.c
                new_lines.append(line)
                continue
        else:
            if '*/' in line:
                in_block_comment = False
            new_lines.append(line)
            continue
        
        # Handle #define lines that may contain runtime expressions
        is_define = stripped.startswith('#define ')
        
        if stripped.startswith('#') and not is_define:
            new_lines.append(line)
            continue
        
        # Pattern: (WITH_GMP) ? A : B -> B
        line = re.sub(r'\(WITH_GMP\)\s*\?\s*([^:]+)\s*:\s*([^;\n,)]+)', r'\2', line)
        
        # Pattern: (!WITH_GMP) ? A : B -> A
        line = re.sub(r'\(!WITH_GMP\)\s*\?\s*([^:]+)\s*:\s*([^;\n,)]+)', r'\1', line)
        
        # Pattern: ((!WITH_GMP) && (X)) -> (X)
        # Use string replacement for robustness with nested parentheses
        line = line.replace('((!WITH_GMP) && ', '(')
        line = line.replace('(!WITH_GMP) && ', '')
        
        # Pattern: if (WITH_GMP) return(false);  -> remove entire line
        if re.match(r'^\s*if\s*\(\s*WITH_GMP\s*\)\s*return\s*\(\s*false\s*\)\s*;\s*$', line):
            line = ''
        
        # Pattern: if (WITH_GMP) return_null(sc, expr); -> remove entire line
        if re.match(r'^\s*if\s*\(\s*WITH_GMP\s*\)\s*return_null\s*\(', line):
            line = ''
        
        # Pattern: if ((!WITH_GMP) && (cond)) -> if (cond)
        # The && replacement above handles most cases; catch any remaining
        line = re.sub(r'if\s*\(\s*\(\s*!WITH_GMP\s*\)\s*&&\s*\(([^)]+)\)\s*\)', r'if (\1)', line)
        
        new_lines.append(line)
    
    return new_lines


def apply_batch(lines, batch_size, max_lines, dry_run):
    """
    Apply one batch of preprocessor block removals.
    Returns (new_lines, num_processed, stats).
    """
    blocks = find_preproc_blocks(lines)
    
    # Classify and filter only WITH_GMP blocks
    gmp_blocks = []
    for b in blocks:
        action = classify_block(b)
        if action != 'none':
            b['action'] = action
            b['simplified'] = preprocess_condition(b['cond'])
            gmp_blocks.append(b)
    
    # Select first batch_size blocks that fit within max_lines
    selected = []
    total_lines = 0
    for b in gmp_blocks[:batch_size]:
        block_lines = b['end'] - b['start'] + 1
        if total_lines + block_lines > max_lines and selected:
            break
        selected.append(b)
        total_lines += block_lines
    
    if not selected:
        return lines, 0, {}
    
    # Sort by end line descending so deletions don't affect earlier line numbers
    selected.sort(key=lambda b: b['end'], reverse=True)
    
    stats = {'delete': 0, 'unwrap': 0, 'replace': 0, 'total_lines': total_lines}
    for b in selected:
        stats[b['action']] += 1
    
    if dry_run:
        print(f"[DRY RUN] Would process {len(selected)} blocks:")
        for b in selected:
            print(f"  Lines {b['start']+1}-{b['end']+1}: {b['action']} (cond: {b['cond']!r})")
        return lines, len(selected), stats
    
    new_lines = remove_preproc_blocks(lines, selected)
    return new_lines, len(selected), stats


def main():
    parser = argparse.ArgumentParser(description='Remove WITH_GMP conditionals from s7.c')
    parser.add_argument('file', type=Path, help='Path to s7.c')
    parser.add_argument('--batch-size', type=int, default=15, help='Max blocks per batch')
    parser.add_argument('--max-lines', type=int, default=500, help='Max lines affected per batch')
    parser.add_argument('--dry-run', action='store_true', help='Show what would be done')
    parser.add_argument('--runtime', action='store_true', help='Only fix runtime expressions')
    parser.add_argument('--preprocessor', action='store_true', help='Only process preprocessor blocks')
    args = parser.parse_args()
    
    lines = args.file.read_text(encoding='utf-8').splitlines(keepends=True)
    
    if args.runtime:
        new_lines = fix_runtime_expressions(lines)
        if args.dry_run:
            print("[DRY RUN] Runtime expressions would be fixed")
        else:
            args.file.write_text(''.join(new_lines), encoding='utf-8')
            print("Runtime expressions fixed.")
        return
    
    if args.preprocessor:
        new_lines, num, stats = apply_batch(lines, args.batch_size, args.max_lines, args.dry_run)
        if args.dry_run:
            print(f"[DRY RUN] {num} blocks would be processed")
        else:
            args.file.write_text(''.join(new_lines), encoding='utf-8')
            print(f"Processed {num} blocks: {stats}")
        return
    
    # Default: do both
    lines = fix_runtime_expressions(lines)
    new_lines, num, stats = apply_batch(lines, args.batch_size, args.max_lines, args.dry_run)
    if args.dry_run:
        print(f"[DRY RUN] {num} blocks would be processed")
    else:
        args.file.write_text(''.join(new_lines), encoding='utf-8')
        print(f"Processed {num} blocks: {stats}")


if __name__ == '__main__':
    main()
