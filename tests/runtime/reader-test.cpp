#include "runtime/artifact.hpp"
#include "runtime/reader.hpp"
#include "runtime/runtime.hpp"
#include "runtime/standard_primitives.hpp"

#include <cassert>
#include <fstream>
#include <iterator>
#include <stdexcept>
#include <utility>

using namespace goldfish::runtime;

int main() {
    Runtime runtime;
    Evaluator& evaluator = runtime.evaluator();
    install_runtime_primitives(evaluator);

    TinyReader reader(
        evaluator,
        "; comment\n(begin (define x 40) (+ x 2)) '(a . b) \"line\\ntext\" #(1 #\\a)");
    Value program = *reader.read();
    assert(evaluator.eval(program).as_integer() == 42);

    Value dotted = *reader.read();
    assert(dotted.as_object<PairObject>()->car == evaluator.symbol("quote"));
    Value quoted = evaluator.eval(dotted);
    assert(quoted.as_object<PairObject>()->cdr == evaluator.symbol("b"));

    Value string = *reader.read();
    assert(evaluator.string_value(string) == "line\ntext");
    Value vector = *reader.read();
    assert(evaluator.vector_values(vector).size() == 2);
    assert(evaluator.character_value(evaluator.vector_values(vector)[1]) == U'a');
    assert(!reader.read());

    for (const auto& shorthand : {std::pair<const char*, const char*>("#'x", "syntax"),
                                  {"#`x", "quasisyntax"},
                                  {"#,x", "unsyntax"},
                                  {"#,@x", "unsyntax-splicing"}}) {
        TinyReader syntax_reader(evaluator, shorthand.first);
        Value form = *syntax_reader.read();
        assert(form.as_object<PairObject>()->car ==
               evaluator.symbol(shorthand.second));
    }

    TinyReader unicode_character_reader(evaluator, "#\\x3007");
    assert(evaluator.character_value(*unicode_character_reader.read()) ==
           static_cast<char32_t>(0x3007));
    TinyReader utf8_character_reader(evaluator, "#\\µ");
    assert(evaluator.character_value(*utf8_character_reader.read()) ==
           static_cast<char32_t>(0x00b5));

    Value read = evaluator.eval(evaluator.symbol("read"));
    Value port = evaluator.apply_values(
        evaluator.eval(evaluator.symbol("open-input-string")),
        {evaluator.string(" ; port comment\n(1 . 2) 7")})[0];
    Value first = evaluator.apply_values(read, {port})[0];
    assert(first.as_object<PairObject>()->car.as_integer() == 1);
    assert(first.as_object<PairObject>()->cdr.as_integer() == 2);
    assert(evaluator.apply_values(read, {port})[0].as_integer() == 7);
    assert(evaluator.apply_values(read, {port})[0].is_object());
    assert(evaluator.apply_values(
               evaluator.eval(evaluator.symbol("eof-object?")),
               {evaluator.apply_values(read, {port})[0]})[0].as_boolean());

    for (const auto& numeric : {
             std::pair<const char*, const char*>{"3.14", "3.14"},
             {"0.1", "0.1"},
             {"+inf.0", "+inf.0"},
             {"+nan.0", "+nan.0"},
             {"#x1.8", "3/2"},
             {"#xA+Bi", "10+11i"},
             {"4903089/2", "4903089/2"},
             {"123456789012345678901234567890", "123456789012345678901234567890"},
             {"1+2i", "1+2i"}}) {
        TinyReader numeric_reader(evaluator, numeric.first);
        Value value = *numeric_reader.read();
        assert(is_number(value));
        assert(number_to_string(value) == numeric.second);
    }
    TinyReader exact_sum_reader(evaluator, "(+ 1/3 1/6)");
    assert(number_to_string(evaluator.eval(*exact_sum_reader.read())) == "1/2");
    TinyReader inexact_sum_reader(evaluator, "(+ 1.25 2.5)");
    assert(number_to_string(evaluator.eval(*inexact_sum_reader.read())) == "3.75");
    TinyReader big_sum_reader(evaluator, "(+ 9223372036854775807 1)");
    assert(number_to_string(evaluator.eval(*big_sum_reader.read())) == "9223372036854775808");
    TinyReader big_modulo_reader(
        evaluator,
        "(modulo 123456789012345678901234567890 256)");
    assert(number_to_string(evaluator.eval(*big_modulo_reader.read())) ==
           "210");
    TinyReader big_quotient_reader(
        evaluator,
        "(quotient 123456789012345678901234567890 256)");
    std::string big_quotient = number_to_string(
        evaluator.eval(*big_quotient_reader.read()));
    assert(big_quotient == "482253082079475308207947530");
    TinyReader big_gcd_reader(evaluator, "(gcd 12345678901234567890 30)");
    assert(number_to_string(evaluator.eval(*big_gcd_reader.read())) == "30");
    TinyReader big_lcm_reader(evaluator, "(lcm 9223372036854775807 3)");
    assert(number_to_string(evaluator.eval(*big_lcm_reader.read())) ==
           "27670116110564327421");
    TinyReader big_sqrt_reader(
        evaluator, "(exact-integer-sqrt 10000000000000000000000000000000000000000)");
    assert(number_to_string(evaluator.eval(*big_sqrt_reader.read())) ==
           "100000000000000000000");
    TinyReader complex_sum_reader(evaluator, "(+ 1+2i 3+4i)");
    assert(number_to_string(evaluator.eval(*complex_sum_reader.read())) == "4+6i");
    TinyReader unit_imaginary_reader(evaluator, "1+i");
    assert(number_to_string(*unit_imaginary_reader.read()) == "1+1i");
    TinyReader imaginary_unit_reader(evaluator, "+i");
    assert(number_to_string(*imaginary_unit_reader.read()) == "0+1i");
    TinyReader identifier_i_reader(evaluator, "i");
    assert(identifier_i_reader.read()->as_object<SymbolObject>()->name == "i");
    TinyReader exact_sqrt_reader(evaluator, "(sqrt 1/4)");
    assert(number_to_string(evaluator.eval(*exact_sqrt_reader.read())) == "1/2");
    TinyReader negative_sqrt_reader(evaluator, "(sqrt -1)");
    assert(number_to_string(evaluator.eval(*negative_sqrt_reader.read())) ==
           "0.0+1.0i");
    TinyReader exact_negative_power_reader(evaluator, "(expt 2 -10)");
    assert(number_to_string(evaluator.eval(*exact_negative_power_reader.read())) ==
           "1/1024");
    TinyReader inexact_add_reader(evaluator, "(+ 0.1 0.2)");
    assert(number_to_string(evaluator.eval(*inexact_add_reader.read())) ==
           "0.30000000000000004");
    TinyReader inexact_div_zero_reader(evaluator, "(/ 1.0 0.0)");
    assert(number_to_string(evaluator.eval(*inexact_div_zero_reader.read())) ==
           "+inf.0");
    TinyReader exact_round_reader(evaluator, "(round 5/2)");
    assert(number_to_string(evaluator.eval(*exact_round_reader.read())) == "2");
    TinyReader rationalize_reader(evaluator, "(rationalize 0.33 0.01)");
    assert(number_to_string(evaluator.eval(*rationalize_reader.read())) ==
           "1/3");
    TinyReader log_exact_reader(evaluator, "(log 2 4)");
    assert(number_to_string(evaluator.eval(*log_exact_reader.read())) ==
           "1/2");
    TinyReader inexact_floor_reader(evaluator, "(floor -1.2)");
    assert(number_to_string(evaluator.eval(*inexact_floor_reader.read())) == "-2.0");
    TinyReader exp_zero_reader(evaluator, "(exp 0)");
    assert(number_to_string(evaluator.eval(*exp_zero_reader.read())) == "1.0");
    TinyReader negative_reader(evaluator, "(negative? -12345678901234567890)");
    assert(evaluator.eval(*negative_reader.read()).as_boolean());
    TinyReader rectangular_reader(evaluator, "(make-rectangular 2 3)");
    assert(number_to_string(evaluator.eval(*rectangular_reader.read())) ==
           "2+3i");
    TinyReader inexact_zero_imag_reader(
        evaluator, "(make-rectangular 1 0.0)");
    Value inexact_zero_imag = evaluator.eval(*inexact_zero_imag_reader.read());
    assert(number_to_string(inexact_zero_imag) == "1.0");
    TinyReader inexact_zero_exactness_reader(
        evaluator, "(exact? (make-rectangular 1 0.0))");
    assert(!evaluator.eval(*inexact_zero_exactness_reader.read()).as_boolean());
    TinyReader exact_magnitude_reader(evaluator, "(magnitude 3+4i)");
    assert(number_to_string(evaluator.eval(*exact_magnitude_reader.read())) ==
           "5");
    TinyReader nan_equality_reader(evaluator, "(= +nan.0 +nan.0)");
    assert(!evaluator.eval(*nan_equality_reader.read()).as_boolean());
    TinyReader nan_order_reader(evaluator, "(<= +nan.0 1.0)");
    assert(!evaluator.eval(*nan_order_reader.read()).as_boolean());

    // Exercise native primitives independently of the Scheme ABI adapters.
    for (const char* expression : {
             R"((= (string-length "Aé中🐟") 4))",
             R"((char=? (string-ref "Aé中🐟" 2) #\中))",
             R"((equal? (string->list "é中🐟") (list #\é #\中 #\🐟)))",
             R"((equal? (string #\é #\中 #\🐟) "é中🐟"))",
             R"((let ((node (cons 1 2)))
                    (let ((hash (g-identity-hash node)))
                        (set-car! node 3)
                        (= hash (g-identity-hash node)))))",
             R"((let ((p (open-input-string "é中🐟")))
                    (equal? (list (peek-char p) (read-char p) (read-string 1 p)
                                  (read-char p) (eof-object? (read-char p)))
                            (list #\é #\é "中" #\🐟 #t))))",
             R"((begin (define unicode-seen '())
                    (string-for-each
                        (lambda (c) (set! unicode-seen (cons c unicode-seen)))
                        "é中🐟")
                    (equal? unicode-seen (list #\🐟 #\中 #\é))))",
             R"((begin (define unicode-k #f) (define unicode-resumed #f)
                    (define unicode-seen '())
                    (string-for-each
                        (lambda (c)
                            (set! unicode-seen (cons c unicode-seen))
                            (if (char=? c #\中)
                                (call/cc (lambda (k) (set! unicode-k k))) #f))
                        "é中🐟")
                    (if unicode-resumed
                        (equal? unicode-seen (list #\🐟 #\🐟 #\中 #\é))
                        (begin (set! unicode-resumed #t) (unicode-k #f)))))"}) {
        TinyReader unicode_reader(evaluator, expression);
        Value result;
        try {
            result = evaluator.eval(*unicode_reader.read());
        } catch (const RaisedValue&) {
            throw std::runtime_error(std::string("Unicode primitive raised: ") + expression);
        }
        if (!result.is_boolean() || !result.as_boolean())
            throw std::runtime_error(std::string("Unicode primitive regression: ") + expression);
    }

    Value file_port = evaluator.apply_values(
        evaluator.eval(evaluator.symbol("open-input-file")),
        {evaluator.string("tests/runtime/fixtures/lowered-artifact.scm")})[0];
    assert(evaluator.apply_values(read, {file_port})[0].is_object());
    evaluator.apply_values(evaluator.eval(evaluator.symbol("close-input-port")),
                           {file_port});
    try {
        (void)evaluator.apply_values(read, {file_port});
        assert(false);
    } catch (const std::runtime_error&) {
    }

    ArtifactLoader loader(evaluator);
    assert(loader.load_file("tests/runtime/fixtures/lowered-artifact.scm")
               .as_integer() == 42);
    assert(loader.load_gfo_file("tests/runtime/fixtures/lowered-artifact.gfo")
               .as_integer() == 42);

    std::ifstream kernel_file("goldfish/expander/kernel-combined.scm");
    assert(kernel_file);
    std::string kernel_source((std::istreambuf_iterator<char>(kernel_file)),
                              std::istreambuf_iterator<char>());
    TinyReader kernel_reader(evaluator, std::move(kernel_source));
    Value kernel = *kernel_reader.read();
    assert(kernel.as_object<PairObject>()->car == evaluator.symbol("begin"));
    assert(!kernel_reader.read());
}
