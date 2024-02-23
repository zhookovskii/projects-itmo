#include "LN.h"

#include <iostream>

#define LLONG_MAX 9223372036854775807

#define LLONG_MIN (-9223372036854775808)

const LN ZERO = 0LL;

const LN ONE = 1LL;

const LN NOT_A_NUMBER = std::string_view("NaN");

LN::LN(long long number)
{
	sign = number >= 0 ? 0 : 1;
	if (!number)
	{
		digits.push_back(0);
	}
	else
	{
		while (llabs(number) > 0)
		{
			digits.push_back(llabs(number) % 10);
			number /= 10;
		}
	}
}

LN::LN(std::string_view stringView)
{
	char sgn = stringView[0];
	sign = sgn == '-';
	if (sign)
	{
		stringView = stringView.substr(1);
	}
	for (unsigned long long i = stringView.size() - 1; i >= 0 && i < stringView.size(); i--)
	{
		char curr = stringView[i] - '0';
		digits.push_back(curr);
	}
	removeLeadingZeroes();
	LN th = *this;
	if (-th == ZERO && sign)
	{
		sign = 0;
	}
}

LN::LN(const char *string)
{
	char sgn = string[0];
	sign = sgn == '-';
	unsigned long long start = 0LL;
	if (sign)
	{
		start = 1LL;
	}
	char curr = string[start];
	stack< char > nums(0);
	while (curr >= '0' && curr <= '9')
	{
		char number = curr - '0';
		nums.push_back(number);
		curr = string[++start];
	}
	for (unsigned long long i = nums.write; i >= 0 && i <= nums.write; i--)
	{
		digits.push_back(nums.data[i]);
	}
	removeLeadingZeroes();
	LN th = *this;
	if (-th == ZERO && sign)
	{
		sign = 0;
	}
}

LN::LN(const LN &a)
{
	digits = a.digits;
	sign = a.sign;
}

LN::LN(LN &&a)
{
	digits = a.digits;
	sign = a.sign;
}

LN::LN(const stack< char > &dgts, const short sgn)
{
	digits = dgts;
	sign = sgn;
}

LN operator""_LN(const char *string)
{
	LN res(string);
	return res;
}

LN &LN::operator=(const LN &a)
{
	digits = a.digits;
	sign = a.sign;
	return *this;
}

LN &LN::operator=(LN &&a)
{
	digits = a.digits;
	sign = a.sign;
	return *this;
}

LN LN::operator+(const LN &a)
{
	LN temp = a;
	LN th = *this;
	if (temp.checkForNan() || th.checkForNan())
	{
		return NOT_A_NUMBER;
	}
	if (th == ZERO)
	{
		return temp;
	}
	if (temp == ZERO)
	{
		return th;
	}
	if (sign && a.sign)
	{
		return -(-th + (-temp));
	}
	else if (sign)
	{
		return temp - -th;
	}
	else if (a.sign)
	{
		return th - -temp;
	}
	if (temp.digits.write > th.digits.write)
	{
		while (temp.digits.write != th.digits.write)
		{
			th.digits.push_back(0);
		}
	}
	if (th.digits.write > temp.digits.write)
	{
		while (temp.digits.write != th.digits.write)
		{
			temp.digits.push_back(0);
		}
	}
	char carry = 0;
	for (unsigned long long i = 0; i <= th.digits.write; i++)
	{
		char curr_a = th.digits.data[i];
		char curr_b = temp.digits.data[i];
		char curr_res = curr_a + curr_b;
		th.digits.data[i] = (curr_res + carry) % 10;
		carry = (curr_res + carry) > 9 ? 1 : 0;
	}
	if (carry)
	{
		th.digits.push_back(1);
	}
	th.removeLeadingZeroes();
	return th;
}

LN LN::operator-(const LN &a)
{
	LN temp = a;
	LN th = *this;
	if (temp.checkForNan() || th.checkForNan())
	{
		return NOT_A_NUMBER;
	}
	if (th == temp)
	{
		return ZERO;
	}
	if (a.sign == 1)
	{
		return th + -temp;
	}
	if (sign == 1)
	{
		return -(-th + temp);
	}
	if (th < temp)
	{
		return -(temp - th);
	}
	char carry = 0;
	for (unsigned long long i = 0; i <= temp.digits.write || carry; i++)
	{
		signed char curr_res = (th.digits.data[i] - (carry + (i <= temp.digits.write ? temp.digits.data[i] : 0)));
		if (curr_res < 0)
		{
			carry = 1;
		}
		else
		{
			carry = 0;
		}
		if (carry)
		{
			curr_res += 10;
		}
		th.digits.data[i] = curr_res;
	}
	th.removeLeadingZeroes();
	return th;
}

LN LN::operator*(const LN &a)
{
	LN temp = a;
	LN th = *this;
	if (th.checkForNan() || temp.checkForNan())
	{
		return NOT_A_NUMBER;
	}
	if (temp == ZERO || th == ZERO)
	{
		return ZERO;
	}
	short product_sign = th.sign != a.sign;
	unsigned long long res_write = th.digits.write + temp.digits.write + 1;
	stack< char > res(0);
	res.isEmpty = false;
	for (unsigned long long i = 0; i <= res_write; i++)
	{
		res.push_back(0);
	}
	for (unsigned long long i = 0; i <= th.digits.write; i++)
	{
		char carry = 0;
		for (unsigned long long j = 0; j <= temp.digits.write || carry; j++)
		{
			char curr_res = res.data[i + j] + th.digits.data[i] * (j <= temp.digits.write ? temp.digits.data[j] : 0) + carry;
			res.data[i + j] = curr_res % 10;
			carry = curr_res / 10;
		}
	}
	LN product = LN(res, product_sign);
	product.removeLeadingZeroes();
	return product;
}

LN LN::operator/(const LN &a)
{
	LN temp = a;
	LN th = *this;
	if (th.checkForNan() || temp.checkForNan())
	{
		return NOT_A_NUMBER;
	}
	if (temp == ZERO)
	{
		return NOT_A_NUMBER;
	}
	if (th.sign == temp.sign && th.sign)
	{
		return -th / -temp;
	}
	else if (th.sign != temp.sign && th.sign)
	{
		return -(-th / temp);
	}
	else if (th.sign != temp.sign && temp.sign)
	{
		return -(th / -temp);
	}
	if (th < temp)
	{
		return ZERO;
	}
	if (th == temp)
	{
		return ONE;
	}
	unsigned long long res_write = th.digits.write - temp.digits.write + 1;
	stack< char > res(0);
	res.isEmpty = false;
	for (unsigned long long i = 0; i <= res_write; i++)
	{
		res.push_back(0);
	}
	LN division = LN(res, 0);
	for (unsigned long long i = res_write; i >= 0 && i <= res_write; i--)
	{
		while (temp * division <= th)
		{
			division.digits.data[i]++;
		}
		division.digits.data[i]--;
	}
	division.removeLeadingZeroes();
	return division;
}

LN LN::operator%(const LN &a)
{
	LN th = *this;
	LN temp = a;
	LN res = th - ((th / temp) * temp);
	return res;
}

LN LN::operator~()
{
	LN th = *this;
	if (th.checkForNan())
	{
		return NOT_A_NUMBER;
	}
	if (th < ZERO)
	{
		return NOT_A_NUMBER;
	}
	if (th == ZERO || th == ONE)
	{
		return th;
	}
	unsigned long long res_write = th.digits.write;
	stack< char > res(0);
	res.isEmpty = false;
	for (unsigned long long i = 0; i <= res_write; i++)
	{
		res.push_back(0);
	}
	LN root = LN(res, 0);
	for (unsigned long long i = res_write; i >= 0 && i <= res_write; i--)
	{
		while (root * root <= th)
		{
			root.digits.data[i]++;
		}
		root.digits.data[i]--;
	}
	root.removeLeadingZeroes();
	return root;
}

LN LN::operator-()
{
	LN th = *this;
	if (th.checkForNan())
	{
		return NOT_A_NUMBER;
	}
	if (th == ZERO)
	{
		return th;
	}
	th.sign = (th.sign + 1) % 2;
	return th;
}

LN &LN::operator+=(const LN &a)
{
	LN th = *this;
	LN temp = a;
	if (th.checkForNan() || temp.checkForNan())
	{
		*this = NOT_A_NUMBER;
	}
	else
	{
		*this = temp + th;
	}
	return *this;
}

LN &LN::operator-=(const LN &a)
{
	LN th = *this;
	LN temp = a;
	if (th.checkForNan() || temp.checkForNan())
	{
		*this = NOT_A_NUMBER;
	}
	else
	{
		*this = temp - th;
	}
	return *this;
}

LN &LN::operator*=(const LN &a)
{
	LN th = *this;
	LN temp = a;
	if (th.checkForNan() || temp.checkForNan())
	{
		*this = NOT_A_NUMBER;
	}
	else
	{
		*this = temp * th;
	}
	return *this;
}

LN &LN::operator/=(const LN &a)
{
	LN th = *this;
	LN temp = a;
	if (th.checkForNan() || temp.checkForNan())
	{
		*this = NOT_A_NUMBER;
	}
	else
	{
		*this = temp / th;
	}
	return *this;
}

LN &LN::operator%=(const LN &a)
{
	LN th = *this;
	LN temp = a;
	if (th.checkForNan() || temp.checkForNan())
	{
		*this = NOT_A_NUMBER;
	}
	else
	{
		*this = temp % th;
	}
	return *this;
}

bool LN::operator==(const LN &a)
{
	LN temp = a;
	LN th = *this;
	if (temp.checkForNan() || th.checkForNan())
	{
		return false;
	}
	if (th.sign != temp.sign)
	{
		return false;
	}
	if (temp.digits.write != th.digits.write)
	{
		return false;
	}
	for (unsigned long long i = 0; i <= th.digits.write; i++)
	{
		if (temp.digits.data[i] != th.digits.data[i])
		{
			return false;
		}
	}
	return true;
}

bool LN::operator<(const LN &a)
{
	LN temp = a;
	LN th = *this;
	if (th.checkForNan() || temp.checkForNan())
	{
		return false;
	}
	if (th == temp)
	{
		return false;
	}
	if (th.sign == 1 && th.sign == temp.sign)
	{
		return (-temp < -th);
	}
	if (temp.sign < th.sign)
	{
		return true;
	}
	if (temp.sign > th.sign)
	{
		return false;
	}
	if (th.digits.write < temp.digits.write)
	{
		return true;
	}
	if (th.digits.write > temp.digits.write)
	{
		return false;
	}
	for (unsigned long long i = th.digits.write; i >= 0 && i <= th.digits.write; i--)
	{
		if (th.digits.data[i] != temp.digits.data[i])
		{
			return th.digits.data[i] < temp.digits.data[i];
		}
	}
	return false;
}

bool LN::operator<=(const LN &a)
{
	LN temp = a;
	LN th = *this;
	if (th.checkForNan() || temp.checkForNan())
	{
		return false;
	}
	return th == temp || th < temp;
}

bool LN::operator>(const LN &a)
{
	LN temp = a;
	LN th = *this;
	if (temp.checkForNan() || th.checkForNan())
	{
		return false;
	}
	return !(th <= temp);
}

bool LN::operator>=(const LN &a)
{
	LN temp = a;
	LN th = *this;
	if (temp.checkForNan() || th.checkForNan())
	{
		return false;
	}
	return !(th < temp);
}

bool LN::operator!=(const LN &a)
{
	LN temp = a;
	LN th = *this;
	if (temp.checkForNan() || th.checkForNan())
	{
		return true;
	}
	return !(th == temp);
}

void LN::removeLeadingZeroes()
{
	while (digits.peek() == 0 && !digits.isEmpty)
	{
		digits.pop();
	}
}

LN::operator long long() const
{
	long long mult = 1LL;
	long long ans = 0LL;
	for (unsigned long long i = 0; i <= digits.write; i++)
	{
		if (sign)
		{
			if (ans < LLONG_MIN + digits.data[i] * mult)
			{
				throw std::bad_cast();
			}
			else
			{
				ans -= digits.data[i] * mult;
			}
		}
		else
		{
			if (ans > LLONG_MAX - digits.data[i] * mult)
			{
				throw std::bad_cast();
			}
			else
			{
				ans += digits.data[i] * mult;
			}
		}
		mult *= 10LL;
	}
	return ans;
}

LN::operator bool() const
{
	LN th = *this;
	if (th != ZERO)
	{
		return true;
	}
	return false;
}

std::ostream &operator<<(std::ostream &out, const LN &a)
{
	LN temp = a;
	if (temp.sign)
	{
		out << '-';
	}
	for (unsigned long long i = temp.digits.write; i >= 0 && i <= temp.digits.write; i--)
	{
		char curr = temp.digits.data[i] + '0';
		out << curr;
	}
	return out;
}

bool LN::checkForNan()
{
	LN th = *this;
	if (th.digits.write < 2 || th.digits.isEmpty)
	{
		return false;
	}
	char fst = th.digits.data[0] + '0';
	char snd = th.digits.data[1] + '0';
	char trd = th.digits.data[2] + '0';
	if (fst == 'N' && snd == 'a' && trd == 'N')
	{
		return true;
	}
	return false;
}