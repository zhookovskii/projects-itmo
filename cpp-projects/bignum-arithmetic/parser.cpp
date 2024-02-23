#include "parser.h"

#include <functional>
#include <map>
#include <set>
#include <string>

std::set< std::string > binary_set{ "+", "-", "*", "/", "%", "<", "<=", ">=", ">", "==", "!=", "+=", "-=", "*=", "/=" };

std::set< std::string > unary_set{ "~", "_" };

std::map< std::string, std::function< LN(LN, LN) > > map{
	{ "+", std::plus() },		 { "-", std::minus() },			 { "*", std::multiplies() },	{ "/", std::divides() },
	{ "%", std::modulus() },	 { "==", std::equal_to() },		 { "!=", std::not_equal_to() }, { "<", std::less() },
	{ "<=", std::less_equal() }, { ">=", std::greater_equal() }, { ">", std::greater() },		{ "+=", std::plus() },
	{ "-=", std::minus() },		 { "*=", std::multiplies() },	 { "/=", std::divides() }
};

parser::parser(const char* input)
{
	in.open(input);
	if (!in)
	{
		throw input_file_error(input);
	}
}

parser::~parser()
{
	if (in)
	{
		in.close();
	}
}

std::stack< LN > parser::parse()
{
	std::string str;
	while (std::getline(in, str))
	{
		LN res;
		if (binary_set.count(str))
		{
			LN a = stack.top();
			stack.pop();
			LN b = stack.top();
			stack.pop();
			res = map[str](b, a);
		}
		else if (unary_set.count(str))
		{
			LN a = stack.top();
			stack.pop();
			if (str == "~")
			{
				res = ~a;
			}
			else if (str == "_")
			{
				res = -a;
			}
		}
		else
		{
			res = LN(str);
		}
		stack.push(res);
	}
	return stack;
}
