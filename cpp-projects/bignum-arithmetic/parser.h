#pragma once

#include "LN.h"

#include <iostream>
#include <stack>

class input_file_error : public std::exception
{
  public:
	const char* message;
	input_file_error(const char* message) { this->message = message; }
};

class parser
{
  private:
	std::stack< LN > stack;
	std::ifstream in;

  public:
	parser(const char* input = "");
	~parser();
	std::stack< LN > parse();
};
