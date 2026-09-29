#pragma once

#include <cstdint>
#include <boost/multiprecision/cpp_int.hpp>
#include <utility>
#include <string>

namespace goldfish::runtime {

// Arbitrary precision signed integer used by Scheme's exact numeric tower.
class BigInteger final {
public:
    BigInteger() = default;
    explicit BigInteger(std::int64_t value) : value_(value) {}
    explicit BigInteger(boost::multiprecision::cpp_int value)
        : value_(std::move(value)) {}
    static BigInteger parse(const std::string& digits, unsigned radix = 10);

    bool is_zero() const noexcept { return value_ == 0; }
    bool negative() const noexcept { return value_ < 0; }
    std::string to_string(unsigned radix = 10) const;
    double to_double() const;
    bool fits_int64() const noexcept;
    std::int64_t to_int64() const;
    const boost::multiprecision::cpp_int& native() const noexcept { return value_; }

    friend int compare(const BigInteger& a, const BigInteger& b) {
        return a.value_ < b.value_ ? -1 : a.value_ > b.value_ ? 1 : 0;
    }
    friend BigInteger operator-(const BigInteger& a) { return BigInteger(-a.value_); }
    friend BigInteger operator+(const BigInteger& a, const BigInteger& b) { return BigInteger(a.value_ + b.value_); }
    friend BigInteger operator-(const BigInteger& a, const BigInteger& b) { return BigInteger(a.value_ - b.value_); }
    friend BigInteger operator*(const BigInteger& a, const BigInteger& b) { return BigInteger(a.value_ * b.value_); }
    friend BigInteger operator/(const BigInteger& a, const BigInteger& b) { return BigInteger(a.value_ / b.value_); }
    friend BigInteger operator%(const BigInteger& a, const BigInteger& b) { return BigInteger(a.value_ % b.value_); }

    BigInteger& operator+=(const BigInteger& other) { return *this = *this + other; }
    BigInteger& operator-=(const BigInteger& other) { return *this = *this - other; }
    BigInteger& operator*=(const BigInteger& other) { return *this = *this * other; }

    friend bool operator==(const BigInteger& a, const BigInteger& b) {
        return a.value_ == b.value_;
    }
    friend bool operator!=(const BigInteger& a, const BigInteger& b) { return !(a == b); }
    friend bool operator<(const BigInteger& a, const BigInteger& b) { return compare(a, b) < 0; }
    friend bool operator>(const BigInteger& a, const BigInteger& b) { return compare(a, b) > 0; }
    friend bool operator<=(const BigInteger& a, const BigInteger& b) { return compare(a, b) <= 0; }
    friend bool operator>=(const BigInteger& a, const BigInteger& b) { return compare(a, b) >= 0; }

private:
    boost::multiprecision::cpp_int value_ = 0;
};

struct RealNumber final {
    BigInteger numerator{0};
    BigInteger denominator{1};
    double inexact_value = 0.0;
    bool inexact = false;

    static RealNumber exact(BigInteger numerator, BigInteger denominator = BigInteger(1));
    static RealNumber inexact_real(double value);
    bool is_integer() const;
    bool is_zero() const;
    double to_double() const;
    std::string to_string(unsigned radix = 10) const;
};

struct Number final {
    RealNumber real;
    RealNumber imag;
    bool has_imaginary_part = false;

    static Number exact(BigInteger value);
    static Number rational(BigInteger numerator, BigInteger denominator);
    static Number inexact(double value);
    static Number complex(RealNumber real, RealNumber imag);
    bool is_real() const;
    bool is_integer() const;
    bool is_exact() const;
    bool is_zero() const;
    std::string to_string(unsigned radix = 10) const;
};

int compare(const RealNumber& left, const RealNumber& right);
Number number_add(const Number& left, const Number& right);
Number number_subtract(const Number& left, const Number& right);
Number number_multiply(const Number& left, const Number& right);
Number number_divide(const Number& left, const Number& right);
Number number_negate(const Number& value);
Number number_abs(const Number& value);
Number number_sqrt(const Number& value);
Number number_expt(const Number& base, const Number& exponent);
bool number_equal(const Number& left, const Number& right);
int number_compare(const Number& left, const Number& right);
Number number_from_int64(std::int64_t value);
RealNumber exact_from_double(double value);
bool parse_number(const std::string& text, Number& result, unsigned default_radix = 10);

} // namespace goldfish::runtime
