#pragma once
#include "stack.h"

#include <cstring>
#include <fstream>
#include <iostream>

class LN
{
  private:
	short sign = 0;
	stack< char > digits = stack< char >(0);
	bool checkForNan();
	void removeLeadingZeroes();
	LN(const stack< char > &digits, short sign);

  public:
	LN(long long number = 0);
	LN(const LN &a);
	LN(LN &&a);
	LN(std::string_view stringView);
	LN(const char *string);
	LN &operator=(const LN &a);
	LN &operator=(LN &&a);
	bool operator==(const LN &a);
	bool operator<(const LN &a);
	bool operator<=(const LN &a);
	bool operator>(const LN &a);
	bool operator>=(const LN &a);
	bool operator!=(const LN &a);
	LN operator-();
	LN operator+(const LN &a);
	LN operator-(const LN &a);
	LN operator*(const LN &a);
	LN operator/(const LN &a);
	LN operator%(const LN &a);
	LN operator~();
	LN &operator+=(const LN &a);
	LN &operator-=(const LN &a);
	LN &operator*=(const LN &a);
	LN &operator/=(const LN &a);
	LN &operator%=(const LN &a);
	operator long long() const;
	operator bool() const;
	friend std::ostream &operator<<(std::ostream &out, const LN &a);
};

LN operator""_LN(const char *string);
