#include "runtime/numeric.hpp"

#include <algorithm>
#include <charconv>
#include <complex>
#include <cmath>
#include <cctype>
#include <limits>
#include <stdexcept>

namespace goldfish::runtime {
namespace {

BigInteger abs_integer(BigInteger value) { return value.negative() ? -value : value; }

BigInteger gcd(BigInteger a, BigInteger b) {
    a = abs_integer(std::move(a));
    b = abs_integer(std::move(b));
    while (!b.is_zero()) {
        BigInteger r = a % b;
        a = std::move(b);
        b = std::move(r);
    }
    return a;
}

BigInteger integer_sqrt(const BigInteger& value) {
    const auto& n = value.native();
    if (n < 2) return BigInteger(n);
    boost::multiprecision::cpp_int root = 1;
    while (root * root <= n) root <<= 1;
    while (true) {
        boost::multiprecision::cpp_int next = (root + n / root) >> 1;
        if (next >= root) return BigInteger(std::move(root));
        root = std::move(next);
    }
}

bool bounded_power(const boost::multiprecision::cpp_int& base,
                   std::uint64_t exponent,
                   const boost::multiprecision::cpp_int& limit,
                   boost::multiprecision::cpp_int& result) {
    using boost::multiprecision::cpp_int;
    result = 1;
    cpp_int factor = base;
    const cpp_int over_limit = limit + 1;
    while (exponent != 0) {
        if ((exponent & 1U) != 0) {
            result *= factor;
            if (result > limit) return false;
        }
        exponent >>= 1;
        if (exponent != 0) {
            factor *= factor;
            if (factor > limit) factor = over_limit;
        }
    }
    return true;
}

bool perfect_nth_root(const BigInteger& value, const BigInteger& degree,
                      BigInteger& root) {
    using boost::multiprecision::cpp_int;
    if (value.negative() || degree.negative() || degree.is_zero()) return false;
    const cpp_int& n = value.native();
    if (n <= 1) {
        root = value;
        return true;
    }
    const auto bits = static_cast<std::uint64_t>(boost::multiprecision::msb(n)) + 1;
    if (degree.native() > bits - 1) return false;
    const auto q = degree.native().convert_to<std::uint64_t>();
    cpp_int low = 1;
    cpp_int high = cpp_int(1) << ((bits + q - 1) / q);
    while (low <= high) {
        cpp_int candidate = (low + high) >> 1;
        cpp_int power;
        if (bounded_power(candidate, q, n, power)) {
            if (power == n) {
                root = BigInteger(std::move(candidate));
                return true;
            }
            low = candidate + 1;
        } else {
            high = candidate - 1;
        }
    }
    return false;
}

boost::multiprecision::cpp_int rounded_quotient(
    boost::multiprecision::cpp_int numerator,
    const boost::multiprecision::cpp_int& denominator) {
    using boost::multiprecision::cpp_int;
    cpp_int quotient = numerator / denominator;
    cpp_int remainder = numerator % denominator;
    cpp_int twice_remainder = remainder << 1;
    if (twice_remainder > denominator ||
        (twice_remainder == denominator && (quotient & 1) != 0))
        ++quotient;
    return quotient;
}

double exact_rational_to_double(const BigInteger& numerator,
                                const BigInteger& denominator) {
    using boost::multiprecision::cpp_int;
    const bool negative = numerator.negative();
    cpp_int n = numerator.native();
    if (negative) n = -n;
    const cpp_int& d = denominator.native();
    if (n == 0) return 0.0;

    long long exponent = static_cast<long long>(boost::multiprecision::msb(n)) -
                         static_cast<long long>(boost::multiprecision::msb(d));
    if (exponent >= 0) {
        if (n < (d << static_cast<unsigned long long>(exponent))) --exponent;
    } else if ((n << static_cast<unsigned long long>(-exponent)) < d) {
        --exponent;
    }

    const double infinity = std::numeric_limits<double>::infinity();
    if (exponent > 1023)
        return std::copysign(infinity, negative ? -1.0 : 1.0);

    cpp_int significand;
    int binary_scale;
    if (exponent < -1022) {
        significand = rounded_quotient(n << 1074, d);
        binary_scale = -1074;
    } else {
        const long long shift = 52 - exponent;
        cpp_int scaled_numerator = n;
        cpp_int scaled_denominator = d;
        if (shift >= 0)
            scaled_numerator <<= static_cast<unsigned long long>(shift);
        else
            scaled_denominator <<= static_cast<unsigned long long>(-shift);
        significand = rounded_quotient(std::move(scaled_numerator),
                                       scaled_denominator);
        if (significand == (cpp_int(1) << 53)) {
            significand >>= 1;
            ++exponent;
        }
        if (exponent > 1023)
            return std::copysign(infinity, negative ? -1.0 : 1.0);
        binary_scale = static_cast<int>(exponent - 52);
    }
    double result = std::ldexp(significand.convert_to<double>(), binary_scale);
    return std::copysign(result, negative ? -1.0 : 1.0);
}

bool perfect_square(const BigInteger& value, BigInteger& root) {
    if (value.negative()) return false;
    root = integer_sqrt(value);
    return root * root == value;
}

bool parse_real(const std::string& text, unsigned radix, bool force_exact,
                bool force_inexact, RealNumber& out) {
    if (text.empty()) return false;
    const auto slash = text.find('/');
    if (slash != std::string::npos) {
        if (text.find('/', slash + 1) != std::string::npos) return false;
        try {
            BigInteger n = BigInteger::parse(text.substr(0, slash), radix);
            BigInteger d = BigInteger::parse(text.substr(slash + 1), radix);
            out = RealNumber::exact(std::move(n), std::move(d));
            if (force_inexact) out = RealNumber::inexact_real(out.to_double());
            return true;
        } catch (...) { return false; }
    }
    if (radix != 10) {
        auto dot = text.find('.');
        BigInteger n;
        try {
            if (dot == std::string::npos) {
                n = BigInteger::parse(text, radix);
                out = force_inexact ? RealNumber::inexact_real(n.to_double())
                                    : RealNumber::exact(std::move(n));
                return true;
            }
            if (text.find('.', dot + 1) != std::string::npos) return false;
            bool negative = !text.empty() && text[0] == '-';
            std::size_t sign = (!text.empty() &&
                (text[0] == '-' || text[0] == '+')) ? 1 : 0;
            std::string whole = text.substr(sign, dot - sign);
            std::string fraction = text.substr(dot + 1);
            if (whole.empty() && fraction.empty()) return false;
            std::string digits = whole.empty() ? "0" : whole;
            digits += fraction;
            n = BigInteger::parse(digits, radix);
            if (negative) n = -n;
            BigInteger d(1), base(static_cast<std::int64_t>(radix));
            for (std::size_t i = 0; i < fraction.size(); ++i) d *= base;
            out = RealNumber::exact(std::move(n), std::move(d));
            if (force_inexact) out = RealNumber::inexact_real(out.to_double());
            return true;
        }
        catch (...) { return false; }
    }
    if (text == "+inf.0" || text == "inf.0" || text == "-inf.0") {
        if (force_exact) return false;
        double value = std::numeric_limits<double>::infinity();
        if (text[0] == '-') value = -value;
        out = RealNumber::inexact_real(value);
        return true;
    }
    if (text == "+nan.0" || text == "nan.0" || text == "-nan.0") {
        if (force_exact) return false;
        double value = std::numeric_limits<double>::quiet_NaN();
        if (text[0] == '-') value = std::copysign(value, -1.0);
        out = RealNumber::inexact_real(value);
        return true;
    }
    const bool decimal = text.find_first_of(".eE") != std::string::npos ||
        text == "+inf.0" || text == "-inf.0" || text == "+nan.0" ||
        text == "-nan.0" || text == "inf.0" || text == "nan.0";
    if (!decimal) {
        try {
            BigInteger n = BigInteger::parse(text);
            out = force_inexact ? RealNumber::inexact_real(n.to_double())
                                : RealNumber::exact(std::move(n));
            return true;
        } catch (...) { return false; }
    }
    if (force_exact) {
        std::string mantissa = text;
        int exponent = 0;
        auto ep = mantissa.find_first_of("eE");
        if (ep != std::string::npos) {
            try { exponent = std::stoi(mantissa.substr(ep + 1)); }
            catch (...) { return false; }
            mantissa.resize(ep);
        }
        bool negative = !mantissa.empty() && mantissa[0] == '-';
        if (!mantissa.empty() && (mantissa[0] == '-' || mantissa[0] == '+'))
            mantissa.erase(mantissa.begin());
        auto dot = mantissa.find('.');
        unsigned scale = dot == std::string::npos ? 0
            : static_cast<unsigned>(mantissa.size() - dot - 1);
        if (dot != std::string::npos) mantissa.erase(dot, 1);
        try {
            BigInteger n = BigInteger::parse(mantissa);
            if (negative) n = -n;
            BigInteger d(1);
            int power = static_cast<int>(scale) - exponent;
            if (power < 0) {
                for (int i = 0; i < -power; ++i) n *= BigInteger(10);
            } else {
                for (int i = 0; i < power; ++i) d *= BigInteger(10);
            }
            out = RealNumber::exact(std::move(n), std::move(d));
            return true;
        } catch (...) { return false; }
    }
    try {
        std::size_t used = 0;
        double value = std::stod(text, &used);
        if (used != text.size()) return false;
        out = RealNumber::inexact_real(value);
        return true;
    } catch (...) { return false; }
}

} // namespace


BigInteger BigInteger::parse(const std::string& digits, unsigned radix) {
    if (digits.empty() || radix < 2 || radix > 36)
        throw std::invalid_argument("invalid integer");
    std::size_t i = 0;
    bool negative = false;
    if (digits[i] == '+' || digits[i] == '-') {
        negative = digits[i] == '-';
        if (++i == digits.size()) throw std::invalid_argument("invalid integer");
    }
    boost::multiprecision::cpp_int value = 0;
    for (; i < digits.size(); ++i) {
        unsigned char c = static_cast<unsigned char>(digits[i]);
        unsigned digit = c >= '0' && c <= '9' ? c - '0'
            : c >= 'a' && c <= 'z' ? c - 'a' + 10
            : c >= 'A' && c <= 'Z' ? c - 'A' + 10 : 99;
        if (digit >= radix) throw std::invalid_argument("invalid integer digit");
        value *= radix;
        value += digit;
    }
    return BigInteger(negative ? -value : value);
}

std::string BigInteger::to_string(unsigned radix) const {
    if (radix < 2 || radix > 36) throw std::invalid_argument("invalid radix");
    if (value_ == 0) return "0";
    auto n = value_ < 0 ? -value_ : value_;
    std::string out;
    constexpr char digits[] = "0123456789abcdefghijklmnopqrstuvwxyz";
    while (n != 0) {
        unsigned digit = static_cast<unsigned>((n % radix).convert_to<unsigned>());
        out.push_back(digits[digit]);
        n /= radix;
    }
    if (value_ < 0) out.push_back('-');
    std::reverse(out.begin(), out.end());
    return out;
}

double BigInteger::to_double() const { return value_.convert_to<double>(); }
bool BigInteger::fits_int64() const noexcept {
    static const boost::multiprecision::cpp_int min = std::numeric_limits<std::int64_t>::min();
    static const boost::multiprecision::cpp_int max = std::numeric_limits<std::int64_t>::max();
    return value_ >= min && value_ <= max;
}
std::int64_t BigInteger::to_int64() const {
    if (!fits_int64()) throw std::overflow_error("integer does not fit int64");
    return value_.convert_to<std::int64_t>();
}

RealNumber RealNumber::exact(BigInteger n, BigInteger d) {
    if (d.is_zero()) throw std::domain_error("zero denominator");
    if (d.negative()) { n = -n; d = -d; }
    BigInteger g = gcd(n, d);
    if (!g.is_zero()) { n = n / g; d = d / g; }
    RealNumber r;
    r.numerator = std::move(n);
    r.denominator = std::move(d);
    return r;
}
RealNumber RealNumber::inexact_real(double value) {
    RealNumber r;
    r.inexact = true;
    r.inexact_value = value;
    return r;
}
bool RealNumber::is_integer() const { return inexact ? std::isfinite(inexact_value) && std::trunc(inexact_value) == inexact_value : denominator == BigInteger(1); }
bool RealNumber::is_zero() const { return inexact ? inexact_value == 0.0 : numerator.is_zero(); }
double RealNumber::to_double() const {
    if (inexact) return inexact_value;
    return exact_rational_to_double(numerator, denominator);
}
std::string RealNumber::to_string(unsigned radix) const {
    if (inexact) {
        if (std::isnan(inexact_value)) return "+nan.0";
        if (std::isinf(inexact_value)) return std::signbit(inexact_value) ? "-inf.0" : "+inf.0";
        char buffer[64];
        const double magnitude = std::fabs(inexact_value);
        const bool prefer_scientific = magnitude >= 1e9 ||
                                       (magnitude != 0.0 && magnitude < 1e-5);
        auto converted = std::to_chars(buffer, buffer + sizeof(buffer),
            inexact_value, prefer_scientific ? std::chars_format::scientific
                                             : std::chars_format::general);
        if (converted.ec != std::errc{})
            throw std::runtime_error("could not format inexact number");
        std::string s(buffer, converted.ptr);
        const auto exponent_at = s.find_first_of("eE");
        if (exponent_at != std::string::npos) {
            const int exponent = std::stoi(s.substr(exponent_at + 1));
            if (!prefer_scientific) {
                std::string mantissa = s.substr(0, exponent_at);
                bool negative = !mantissa.empty() && mantissa.front() == '-';
                if (negative) mantissa.erase(mantissa.begin());
                const auto dot = mantissa.find('.');
                std::size_t decimal = dot == std::string::npos
                    ? mantissa.size() : dot;
                if (dot != std::string::npos) mantissa.erase(dot, 1);
                const long position = static_cast<long>(decimal) + exponent;
                std::string fixed;
                if (position <= 0) {
                    fixed = "0." + std::string(static_cast<std::size_t>(-position), '0') + mantissa;
                } else if (static_cast<std::size_t>(position) >= mantissa.size()) {
                    fixed = mantissa + std::string(
                        static_cast<std::size_t>(position) - mantissa.size(), '0') + ".0";
                } else {
                    fixed = mantissa;
                    fixed.insert(static_cast<std::size_t>(position), 1, '.');
                }
                s = negative ? "-" + fixed : std::move(fixed);
            } else {
                const std::string mantissa = s.substr(0, exponent_at);
                s = mantissa + "e" + (exponent >= 0 ? "+" : "") +
                    std::to_string(exponent);
            }
        }
        if (s.find_first_of(".eE") == std::string::npos) s += ".0";
        return s;
    }
    std::string n = numerator.to_string(radix);
    return denominator == BigInteger(1) ? n : n + "/" + denominator.to_string(radix);
}
int compare(const RealNumber& a, const RealNumber& b) {
    if (a.inexact || b.inexact) {
        double x = a.inexact_value, y = b.inexact_value;
        if ((a.inexact && std::isnan(x)) ||
            (b.inexact && std::isnan(y))) return 0;
        const bool infinite_x = a.inexact && std::isinf(x);
        const bool infinite_y = b.inexact && std::isinf(y);
        if (infinite_x && infinite_y)
            return x < y ? -1 : x > y ? 1 : 0;
        if (infinite_x) return x < 0 ? -1 : 1;
        if (infinite_y) return y < 0 ? 1 : -1;
        RealNumber exact_x = a.inexact ? exact_from_double(x) : a;
        RealNumber exact_y = b.inexact ? exact_from_double(y) : b;
        return compare(exact_x, exact_y);
    }
    return compare(a.numerator * b.denominator, b.numerator * a.denominator);
}
Number Number::exact(BigInteger value) { Number n; n.real = RealNumber::exact(std::move(value)); return n; }
Number Number::rational(BigInteger n, BigInteger d) { Number r; r.real = RealNumber::exact(std::move(n), std::move(d)); return r; }
Number Number::inexact(double value) { Number n; n.real = RealNumber::inexact_real(value); return n; }
Number Number::complex(RealNumber r, RealNumber i) { Number n; n.real = std::move(r); n.imag = std::move(i); n.has_imaginary_part = true; return n; }
bool Number::is_real() const { return !has_imaginary_part || imag.is_zero(); }
bool Number::is_integer() const { return is_real() && real.is_integer(); }
bool Number::is_exact() const { return real.inexact == false && (!has_imaginary_part || !imag.inexact); }
bool Number::is_zero() const { return real.is_zero() && (!has_imaginary_part || imag.is_zero()); }
std::string Number::to_string(unsigned radix) const {
    if (!has_imaginary_part) return real.to_string(radix);
    if (imag.is_zero() && !imag.inexact) {
        return real.to_string(radix);
    }
    std::string r = real.to_string(radix), i = imag.to_string(radix);
    if (i[0] != '-') return r + "+" + i + "i";
    return r + i + "i";
}
Number number_from_int64(std::int64_t value) { return Number::exact(BigInteger(value)); }
RealNumber exact_from_double(double value) {
    if (!std::isfinite(value)) throw std::domain_error("non-finite number has no exact representation");
    if (value == 0.0) return RealNumber::exact(BigInteger(0));
    int exponent = 0;
    double fraction = std::frexp(value, &exponent);
    constexpr int precision = std::numeric_limits<double>::digits;
    auto significand = static_cast<std::int64_t>(std::ldexp(fraction, precision));
    BigInteger numerator(significand), denominator(1);
    int shift = exponent - precision;
    BigInteger two(2);
    if (shift >= 0) for (int i = 0; i < shift; ++i) numerator *= two;
    else for (int i = 0; i < -shift; ++i) denominator *= two;
    return RealNumber::exact(std::move(numerator), std::move(denominator));
}

Number number_add(const Number& a, const Number& b) {
    auto add = [](const RealNumber& x, const RealNumber& y) {
        if (x.inexact || y.inexact) return RealNumber::inexact_real(x.to_double() + y.to_double());
        return RealNumber::exact(x.numerator*y.denominator + y.numerator*x.denominator, x.denominator*y.denominator);
    };
    return Number::complex(add(a.real,b.real), add(a.imag,b.imag));
}
Number number_negate(const Number& a) {
    auto neg = [](const RealNumber& x) { return x.inexact ? RealNumber::inexact_real(-x.inexact_value) : RealNumber::exact(-x.numerator,x.denominator); };
    return Number::complex(neg(a.real), neg(a.imag));
}
Number number_subtract(const Number& a, const Number& b) { return number_add(a, number_negate(b)); }
Number number_multiply(const Number& a, const Number& b) {
    auto mul = [](const RealNumber& x, const RealNumber& y) {
        if (x.inexact || y.inexact) return RealNumber::inexact_real(x.to_double()*y.to_double());
        return RealNumber::exact(x.numerator*y.numerator,x.denominator*y.denominator);
    };
    RealNumber real = mul(a.real,b.real), imag = mul(a.real,b.imag);
    RealNumber cross = mul(a.imag,b.real), ii = mul(a.imag,b.imag);
    auto sub = [](const RealNumber& x, const RealNumber& y) { return x.inexact || y.inexact ? RealNumber::inexact_real(x.to_double()-y.to_double()) : RealNumber::exact(x.numerator*y.denominator-y.numerator*x.denominator,x.denominator*y.denominator); };
    auto add = [](const RealNumber& x, const RealNumber& y) { return x.inexact || y.inexact ? RealNumber::inexact_real(x.to_double()+y.to_double()) : RealNumber::exact(x.numerator*y.denominator+y.numerator*x.denominator,x.denominator*y.denominator); };
    return Number::complex(sub(real,ii),add(imag,cross));
}
Number number_divide(const Number& a, const Number& b) {
    if (b.is_zero() && b.is_exact())
        throw std::domain_error("division by zero");
    if (a.is_real() && b.is_real()) {
        if (!a.is_exact() || !b.is_exact())
            return Number::inexact(a.real.to_double() / b.real.to_double());
        return Number::complex(
            RealNumber::exact(a.real.numerator * b.real.denominator,
                              a.real.denominator * b.real.numerator),
            RealNumber::exact(BigInteger(0)));
    }
    auto add = [](const RealNumber& x, const RealNumber& y) { return x.inexact || y.inexact ? RealNumber::inexact_real(x.to_double()+y.to_double()) : RealNumber::exact(x.numerator*y.denominator+y.numerator*x.denominator,x.denominator*y.denominator); };
    auto sub = [](const RealNumber& x, const RealNumber& y) { return x.inexact || y.inexact ? RealNumber::inexact_real(x.to_double()-y.to_double()) : RealNumber::exact(x.numerator*y.denominator-y.numerator*x.denominator,x.denominator*y.denominator); };
    auto mul = [](const RealNumber& x, const RealNumber& y) { return x.inexact || y.inexact ? RealNumber::inexact_real(x.to_double()*y.to_double()) : RealNumber::exact(x.numerator*y.numerator,x.denominator*y.denominator); };
    auto div = [](const RealNumber& x, const RealNumber& y) { return x.inexact || y.inexact ? RealNumber::inexact_real(x.to_double()/y.to_double()) : RealNumber::exact(x.numerator*y.denominator,x.denominator*y.numerator); };
    RealNumber c2 = mul(b.real,b.real), d2 = mul(b.imag,b.imag);
    RealNumber denom = add(c2,d2);
    return Number::complex(div(add(mul(a.real,b.real),mul(a.imag,b.imag)),denom), div(sub(mul(a.imag,b.real),mul(a.real,b.imag)),denom));
}
Number number_abs(const Number& a) {
    if (a.has_imaginary_part && !a.imag.is_zero()) {
        if (a.is_exact()) {
            Number real = Number::complex(a.real,
                RealNumber::exact(BigInteger(0)));
            Number imag = Number::complex(a.imag,
                RealNumber::exact(BigInteger(0)));
            return number_sqrt(number_add(number_multiply(real, real),
                                          number_multiply(imag, imag)));
        }
        return Number::inexact(std::hypot(a.real.to_double(),a.imag.to_double()));
    }
    if (a.has_imaginary_part && !a.is_exact())
        return Number::inexact(std::fabs(a.real.to_double()));
    RealNumber r = a.real;
    if (r.inexact) return Number::inexact(std::fabs(r.inexact_value));
    return Number::complex(RealNumber::exact(abs_integer(r.numerator),r.denominator),RealNumber::exact(BigInteger(0)));
}
Number number_sqrt(const Number& a) {
    if (a.is_real() && a.is_exact()) {
        BigInteger numerator_root, denominator_root;
        if (a.real.numerator.negative()) {
            BigInteger positive_root;
            if (perfect_square(-a.real.numerator, positive_root) &&
                perfect_square(a.real.denominator, denominator_root))
                return Number::complex(
                    RealNumber::inexact_real(0.0),
                    RealNumber::inexact_real(
                        RealNumber::exact(positive_root,
                                          denominator_root).to_double()));
        } else if (perfect_square(a.real.numerator, numerator_root) &&
                   perfect_square(a.real.denominator, denominator_root)) {
            return Number::complex(
                RealNumber::exact(std::move(numerator_root),
                                  std::move(denominator_root)),
                RealNumber::exact(BigInteger(0)));
        }
    }
    if (a.is_real() && a.real.to_double() >= 0) return Number::inexact(std::sqrt(a.real.to_double()));
    double x=a.real.to_double(), y=a.imag.to_double();
    std::complex<double> z(x,y), r=std::sqrt(z);
    return Number::complex(RealNumber::inexact_real(r.real()),RealNumber::inexact_real(r.imag()));
}
Number number_expt(const Number& a, const Number& b) {
    if (a.is_exact() && b.is_integer() && b.is_exact()) {
        BigInteger exponent = b.real.inexact
            ? exact_from_double(b.real.inexact_value).numerator
            : b.real.numerator;
        bool reciprocal = exponent.negative();
        if (reciprocal) exponent = -exponent;
        Number result = Number::exact(BigInteger(1)), base = a;
        const auto& count = exponent.native();
        if (count <= std::numeric_limits<std::uint64_t>::max()) {
            auto n = count.convert_to<std::uint64_t>();
            while (n) {
                if (n & 1) result = number_multiply(result, base);
                n >>= 1;
                if (n) base = number_multiply(base, base);
            }
            if (reciprocal)
                result = number_divide(Number::exact(BigInteger(1)), result);
            return result;
        }
        const Number one = Number::exact(BigInteger(1));
        const Number minus_one = Number::exact(BigInteger(-1));
        if (a.is_zero()) {
            if (reciprocal) throw std::domain_error("division by zero");
            return Number::exact(BigInteger(0));
        }
        if (number_equal(a, one)) return one;
        if (number_equal(a, minus_one))
            return (count & 1) != 0 ? minus_one : one;
    }
    if (a.is_exact() && a.is_real() && b.is_exact() && b.is_real() &&
        !b.real.is_integer()) {
        const BigInteger& degree = b.real.denominator;
        BigInteger numerator_root, denominator_root;
        const bool negative_base = a.real.numerator.negative();
        const BigInteger magnitude = negative_base
            ? -a.real.numerator : a.real.numerator;
        if ((!negative_base || (degree.native() & 1) != 0) &&
            perfect_nth_root(magnitude, degree, numerator_root) &&
            perfect_nth_root(a.real.denominator, degree, denominator_root)) {
            if (negative_base) numerator_root = -numerator_root;
            Number root = Number::rational(std::move(numerator_root),
                                           std::move(denominator_root));
            return number_expt(root, Number::exact(b.real.numerator));
        }
    }
    std::complex<double> x(a.real.to_double(),a.imag.to_double()),
                         y(b.real.to_double(),b.imag.to_double()), z=std::pow(x,y);
    return Number::complex(RealNumber::inexact_real(z.real()),RealNumber::inexact_real(z.imag()));
}
bool number_equal(const Number& a, const Number& b) {
    auto has_nan = [](const RealNumber& value) {
        return value.inexact && std::isnan(value.inexact_value);
    };
    if (has_nan(a.real) || has_nan(a.imag) ||
        has_nan(b.real) || has_nan(b.imag))
        return false;
    return compare(a.real,b.real)==0 && compare(a.imag,b.imag)==0;
}
int number_compare(const Number& a, const Number& b) {
    if (!a.is_real() || !b.is_real()) throw std::domain_error("complex numbers are unordered");
    return compare(a.real,b.real);
}

bool parse_number(const std::string& source, Number& result, unsigned default_radix) {
    if (source.empty()) return false;
    std::string text=source;
    unsigned radix=default_radix;
    bool exact=false, inexact=false;
    bool saw_radix=false, saw_exactness=false;
    while (text.size()>=2 && text[0]=='#') {
        char c=static_cast<char>(std::tolower(static_cast<unsigned char>(text[1])));
        if(c=='b'||c=='o'||c=='d'||c=='x'){
            if (saw_radix) return false;
            saw_radix=true;
            radix=c=='b'?2:c=='o'?8:c=='x'?16:10;
        }
        else if(c=='e'||c=='i'){
            if (saw_exactness) return false;
            saw_exactness=true;
            exact=c=='e';
            inexact=c=='i';
        }
        else break;
        text.erase(0,2);
    }
    const auto at=text.find('@');
    if(at!=std::string::npos){RealNumber r,t;if(!parse_real(text.substr(0,at),radix,exact,inexact,r)||!parse_real(text.substr(at+1),radix,exact,inexact,t))return false;double mag=r.to_double(),ang=t.to_double();result=Number::complex(RealNumber::inexact_real(mag*std::cos(ang)),RealNumber::inexact_real(mag*std::sin(ang)));return true;}
    if (text.back()=='i') {
        std::string body=text.substr(0,text.size()-1);std::size_t split=std::string::npos;
        if (body.empty()) return false;
        for(std::size_t i=1;i<body.size();++i)if((body[i]=='+'||body[i]=='-')&&body[i-1]!='e'&&body[i-1]!='E')split=i;
        std::string rs, is;
        if(split==std::string::npos){rs="0";is=body.empty()||body=="+"?"1":body=="-"?"-1":body;}
        else{rs=body.substr(0,split);is=body.substr(split);if(is=="+")is="1";else if(is=="-")is="-1";}
        RealNumber r,i;if(!parse_real(rs,radix,exact,inexact,r)||!parse_real(is,radix,exact,inexact,i))return false;result=Number::complex(std::move(r),std::move(i));return true;
    }
    RealNumber real;
    if(!parse_real(text,radix,exact,inexact,real))return false;
    result=Number::complex(std::move(real),RealNumber::exact(BigInteger(0)));
    return true;
}

} // namespace goldfish::runtime
