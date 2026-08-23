//
// Copyright (C) 2024-2026 The Goldfish Scheme Authors
// Licensed under the Apache License, Version 2.0 (the "License");
//
// gf_repl.hpp -- interactive REPL components: value history, isocline
// completion/highlighting, meta commands and the read-eval loop.
//
// Included INSIDE namespace goldfish under GOLDFISH_WITH_REPL; expects
// isocline.h and the eval helpers above to be visible.  Do not include
// directly.

#ifdef GOLDFISH_WITH_REPL
struct SymbolInfo {
  std::string name;
  std::string doc;
};
static std::vector<SymbolInfo> cached_symbols;

// UNLIMITED history
// TODO(jinser): 1. programatic value-history procedure api in scheme
//               2. `,option value-history` meta command
static std::vector<gf::pointer> history_values;

inline void
update_symbol_cache (gf::scheme* sc) {
  cached_symbols.clear ();
  gf::pointer cur_env = gf::curlet (sc);
  gf::pointer sym_list= gf::let_to_list (sc, cur_env);
  int        n       = gf::list_length (sc, sym_list);
  for (int i= 0; i < n; ++i) {
    gf::pointer  pair= gf::list_ref (sc, sym_list, i);
    gf::pointer  sym = gf::car (pair);
    gf::pointer  val = gf::cdr (pair);
    const char* name= gf::symbol_name (sym);
    const char* doc = gf::documentation (sc, val);
    cached_symbols.push_back ({name, doc ? doc : ""});
  }
}

inline void
ic_goldfish_eval (gf::scheme* sc, const char* code) {
  int        err_gc_loc= -1, out_gc_loc= -1;
  gf::pointer old_err_port= gf::set_current_error_port (sc, gf::open_output_string (sc));
  if (old_err_port != gf::nil (sc)) err_gc_loc= gf::gc_protect (sc, old_err_port);

  gf::pointer out_port    = gf::open_output_string (sc);
  gf::pointer old_out_port= gf::set_current_output_port (sc, out_port);
  if (old_err_port != gf::nil (sc)) out_gc_loc= gf::gc_protect (sc, old_out_port);

  gf::pointer result= goldfish_eval_through_reader (sc, code);

  const char* display_out= gf::get_output_string (sc, out_port);
  if (display_out && *display_out) {
    std::string out_str= display_out;
    if (!out_str.empty () && out_str.back () == '\n') {
      ic_printf ("%s", display_out);
    }
    else {
      // 用以表示换行符由 REPL 添加
      ic_printf ("%s↩\n", display_out);
    }
  }

  const char* errmsg= gf::get_output_string (sc, gf::current_error_port (sc));

  if (errmsg && *errmsg) {
    string rendered;
    goldfish_render_scheme_error_message (sc, errmsg, rendered);
    ic_printf ("[error]%s[/]", rendered.c_str ());
  }
  if (result) {
    history_values.push_back (result);
    gf::gc_protect (sc, result);
    std::string name   = "$" + std::to_string (history_values.size ());
    // Bind history values in the rootlet: expand-eval evaluates in the
    // expander library inlet, so s7_curlet there would not be rootlet-visible.
    gf::pointer  cur_env= gf::rootlet (sc);
    gf::define (sc, cur_env, gf::make_symbol (sc, name.c_str ()), result);
    // Register the history name as a primitive binding in the session
    // program library too: a strict program resolves identifiers only from
    // its imports, so without this the next (expand-eval '(... $1 ...))
    // would fail with "unbound identifier in program".
    goldfish_eval_through_reader (
        sc, ("(import (goldfish)) (register-program-library-primitive! '" + name + ")").c_str ());

    char* result_str= gf::object_to_c_string (sc, result);
    if (result_str) {
      ic_printf ("%s [gray]=[/] %s\n", name.c_str (), result_str);
      free (result_str);
    }
  }

  gf::close_output_port (sc, gf::current_error_port (sc));
  gf::set_current_error_port (sc, old_err_port);

  if (err_gc_loc != -1) gf::gc_unprotect_at (sc, err_gc_loc);
  if (out_gc_loc != -1) gf::gc_unprotect_at (sc, out_gc_loc);

  update_symbol_cache (sc);
}

inline std::string
get_history_path () {
#ifdef TB_CONFIG_OS_WINDOWS
  const char* appdata= getenv ("APPDATA");
  std::string dir    = appdata ? std::string (appdata) + "\\goldfish" : ".";
  tb_directory_create (dir.c_str ());
  std::string path= dir + "\\history";
#else
  const char* xdg_state= getenv ("XDG_STATE_HOME");
  const char* xdg_data = getenv ("XDG_DATA_HOME");
  const char* home     = getenv ("HOME");
  std::string dir;
  if (xdg_data) {
    dir= std::string (xdg_data) + "/goldfish";
  }
  else if (home) {
    dir= std::string (home) + "/.local/share/goldfish";
  }
  else {
    dir= ".";
  }
  // 可选：创建目录
  tb_directory_create (dir.c_str ());
  std::string path= dir + "/history";
#endif
  return path;
}

inline bool
is_symbol_char (const char* s, long len) {
  int c= (unsigned char) *s;
  return isalnum (c) || strchr ("!$%&*/:<=>?^_~+-.", c);
}

inline void
symbol_completer (ic_completion_env_t* cenv, const char* symbol) {
  constexpr size_t MAXLEN   = 79;
  size_t           input_len= strlen (symbol);
  for (const auto& info : cached_symbols) {
    if (strncmp (info.name.c_str (), symbol, input_len) == 0) {
      const char* doc= nullptr;
      std::string short_doc;
      if (!info.doc.empty ()) {
        if (info.doc.length () > MAXLEN) {
          short_doc= info.doc.substr (0, MAXLEN) + "...";
          doc      = short_doc.c_str ();
        }
        else {
          doc= info.doc.c_str ();
        }
      }
      ic_add_completion_ex (cenv, info.name.c_str (), info.name.c_str (), doc);
    }
  }
}

inline void
goldfish_completer (ic_completion_env_t* cenv, const char* input) {
  ic_complete_word (cenv, input, &symbol_completer, is_symbol_char);
}

inline void
goldfish_highlighter (ic_highlight_env_t* henv, const char* input, void* arg) {
  static const char* keywords[]= {"define",
                                  "lambda",
                                  "if",
                                  "else",
                                  "let",
                                  "let*",
                                  "letrec",
                                  "begin",
                                  "quote",
                                  "set!",
                                  "cond",
                                  "case",
                                  "and",
                                  "or",
                                  "do",
                                  "delay",
                                  "quasiquote",
                                  "unquote",
                                  "unquote-splicing",
                                  NULL};
  long               len       = (long) strlen (input);
  for (long i= 0; i < len;) {
    long tlen;
    if ((tlen= ic_match_any_token (input, i, &ic_char_is_idletter, keywords)) > 0) {
      // 关键字
      ic_highlight (henv, i, tlen, "keyword");
      i+= tlen;
    }
    else if ((tlen= ic_is_token (input, i, &is_symbol_char)) > 0) {
      // 已定义符号

      std::string token (input + i, tlen);
      if (std::any_of (cached_symbols.begin (), cached_symbols.end (),
                       [&] (const SymbolInfo& info) { return info.name == token; })) {
        ic_highlight (henv, i, tlen, "symbol");
      }
      else {
        ic_highlight (henv, i, tlen, nullptr);
      }
      i+= tlen;
    }
    else if ((tlen= ic_is_token (input, i, &ic_char_is_digit)) > 0) {
      // 数字
      ic_highlight (henv, i, tlen, "number");
      i+= tlen;
    }
    else if (input[i] == '#' && (input[i + 1] == 't' || input[i + 1] == 'f')) {
      // 布尔值
      ic_highlight (henv, i, 2, "constant");
      i+= 2;
    }
    else if (input[i] == '"') {
      long start= i;
      i++;
      while (i < len && input[i] != '"') {
        if (input[i] == '\\' && i + 1 < len) i++; // 跳过转义
        i++;
      }
      if (i < len) i++; // 包含结尾引号
      ic_highlight (henv, start, i - start, "string");
    }
    else if (input[i] == ';') {
      // 注释
      long start= i;
      while (i < len && input[i] != '\n')
        i++;
      ic_highlight (henv, start, i - start, "comment");
    }
    else {
      // 其它
      ic_highlight (henv, i, 1, nullptr);
      i++;
    }
  }
}

struct MetaCommand {
  const char* name;
  const char* help;
  bool        exact;

  std::function<bool (const char* input, gf::scheme* sc, const char* arg)> handler;
};

inline bool meta_quit (const char*, gf::scheme*, const char*);
inline bool meta_help (const char*, gf::scheme*, const char*);
inline bool meta_import (const char*, gf::scheme*, const char*);
inline bool meta_apropos (const char*, gf::scheme* sc, const char* arg);
inline bool meta_describe (const char*, gf::scheme* sc, const char* arg);

const MetaCommand commands[]= {
    {",quit", "exit REPL", true, meta_quit},
    {",q", "exit REPL", true, meta_quit},
    {",help", "show this help", true, meta_help},
    {",?", "show this help", true, meta_help},
    {",import", "import Scheme module", false, meta_import},
    {",apropos", "search symbols by substring", false, meta_apropos},
    {",a", "search symbols by substring", false, meta_apropos},
    {",describe", "describe symbol", false, meta_describe},
    {",d", "describe symbol", false, meta_describe},
};
const size_t commands_count= sizeof (commands) / sizeof (commands[0]);

inline bool
meta_quit (const char*, gf::scheme*, const char*) {
  return true;
}

// TODO: ,help <command>
inline bool
meta_help (const char*, gf::scheme*, const char*) {
  ic_printf ("[b]Meta commands:[/]\n");
  for (const auto& cmd : commands) {
    ic_printf ("[b]%-16s[/] %s\n", cmd.name, cmd.help);
  }
  return false;
}

inline bool
meta_import (const char*, gf::scheme* sc, const char* arg) {
  if (!arg || *arg == 0) {
    ic_printf ("[red]Usage:[/] ,import <module>\n");
    return false;
  }
  std::string mod = arg;
  std::string code= "(import " + mod + ")";

  ic_goldfish_eval (sc, code.c_str ());

  return false;
}

inline bool
meta_apropos (const char*, gf::scheme*, const char* arg) {
  if (!arg || !*arg) {
    ic_printf ("[b]Usage:[/] ,apropos <substring>\n");
    return false;
  }
  int found= false;
  for (const auto& info : cached_symbols) {
    if (strstr (info.name.c_str (), arg)) {
      ic_printf ("[b cyan]%s[/] [dim](procedure)[/] %s\n", info.name.c_str (),
                 info.doc.empty () ? "" : info.doc.c_str ());
      found= true;
    }
  }
  if (!found) ic_printf ("[dim]No symbol matches '%s'[/]\n", arg);
  return false;
}

inline bool
meta_describe (const char*, gf::scheme* sc, const char* arg) {
  if (!arg || !*arg) {
    ic_printf ("[b]Usage:[/] ,describe <symbol>\n");
    return false;
  }
  // 查找符号
  gf::pointer sym= gf::make_symbol (sc, arg);

  // 检查是否已定义
  if (!gf::is_defined (sc, gf::symbol_name (sym))) {
    ic_printf ("[dim]Symbol not defined: %s[/]\n", arg);
    return false;
  }
  gf::pointer  val = gf::symbol_value (sc, sym);
  const char* type= gf::object_to_c_string (sc, gf::type_of (sc, val));
  ic_printf ("[b]%s[/] [dim](%s)[/]\n", arg, type);

  if (gf::is_procedure (val)) {
    // 参数信息
    gf::pointer arity   = gf::arity (sc, val);
    gf::int_     min_args= gf::integer (gf::car (arity));
    gf::int_     max_args= gf::integer (gf::cdr (arity));

    std::string max_str= (max_args >= 0x20000000) ? "any" : std::to_string (max_args);
    ic_printf ("  [gray]Arity:[/] min [number]%d[/], max [number]%s[/]\n", min_args, max_str.c_str ());

    gf::pointer sig= gf::signature (sc, val);
    if (sig && !gf::is_null (sc, sig)) {
      char* sig_str= gf::object_to_c_string (sc, sig);
      if (sig_str) {
        ic_printf ("  [gray]Signature:[/] %s\n", sig_str);
        free (sig_str);
      }
    }

    // 文档
    const char* doc= gf::documentation (sc, val);
    if (doc && *doc) {
      ic_printf ("  [gray]Doc:[/] %s\n", doc);
    }
  }
  else {
    char*       val_str= gf::object_to_c_string (sc, val);
    std::string preview;
    if (val_str) {
      preview= std::string (val_str).substr (0, 80);
      if (strlen (val_str) > 80) preview+= "...";
    }
    else {
      preview= "";
    }
    ic_printf ("  [gray]Value:[/] %s\n", preview.c_str ());
    if (val_str) free (val_str);
  }
  return false;
}

inline bool
handle_meta_command (const char* input, gf::scheme* sc) {
  for (const auto& cmd : commands) {
    size_t len= strlen (cmd.name);
    if (cmd.exact) {
      if (strcmp (input, cmd.name) == 0) return cmd.handler (input, sc, nullptr);
    }
    else {
      if (strncmp (input, cmd.name, len) == 0) {
        // 跳过空格
        const char* arg= input + len + 1;
        while (*arg == ' ')
          ++arg;
        return cmd.handler (input, sc, input + len + 1);
      }
    }
  }
  ic_printf ("[red]Unknown meta command:[/] %s\n", input);
  return false;
}

inline void
goldfish_repl (gf::scheme* sc, const string& mode) {
  setlocale (LC_ALL, "C.UTF-8");
  ic_style_def ("kbd", "gray underline");
  ic_style_def ("ic-prompt", "gold");

  // 自定义样式
  ic_style_def ("error", "red");
  ic_style_def ("symbol", "cyan");

  ic_printf ("[b gold]Goldfish Scheme[/] [b plum]%s[/] by LiiiLabs\n"
             "[i]Based on S7 Scheme %s [dim](%s)[/][/]\n",
             GOLDFISH_VERSION, gf::host_version (), gf::host_date ());
  // Display mode info; liii mode shows extra imported libraries
  if (mode == "liii" || mode == "default") {
    ic_printf ("[b]Mode:[/] [b]%s[/] (imports the r7rs-small libraries plus (liii base) "
               "(liii error) (liii string); r7rs imports only (scheme base))\n\n",
               mode.c_str ());
  }
  else {
    ic_printf ("[b]Mode:[/] [b]%s[/]\n\n", mode.c_str ());
  }
  ic_printf ("- Type ',quit' or ',q' to quit. (or use [kbd]ctrl-d[/]).\n"
             "- Type ',help' for REPL commands help.\n"
             "- Press [kbd]F1[/] for help on editing commands.\n"
             "- Use [kbd]shift-tab[/] for multiline input. (or [kbd]ctrl-enter[/], or [kbd]ctrl-j[/])\n"
             "- Use [kbd]ctrl-r[/] to search the history.\n\n");

  auto history_path= get_history_path ();
  ic_set_history (history_path.c_str (), -1);

  ic_set_default_completer (&goldfish_completer, sc);
  ic_set_default_highlighter (&goldfish_highlighter, nullptr);

  //  prompt_marker, continuation_prompt_marker
  ic_set_prompt_marker ("> ", "... ");
  ic_enable_auto_tab (true);
  // 缓存的符号向量，只需要查表，没有必要延迟
  ic_set_hint_delay (0);

  update_symbol_cache (sc);

  while (true) {
    char* input= ic_readline ("gf");
    if (!input) break;
    if (strlen (input) == 0) {
      free (input);
      continue;
    }
    if (input[0] == ',') {
      bool quit= handle_meta_command (input, sc);
      free (input);
      if (quit) break;
      continue;
    }

    ic_goldfish_eval (sc, input);
  }
}
#endif
