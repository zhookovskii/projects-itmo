#include "LN.h"
#include "parser.h"
#include "return_codes.h"

#include <iostream>
#include <stack>
#include <string>

int main(int argc, char** argv)
{
	if (argc != 3)
	{
		fprintf(stderr, "Incorrect parameters given");
		return ERROR_INVALID_PARAMETER;
	}
	std::ifstream fin;
	fin.open(argv[1]);
	if (!fin)
	{
		fprintf(stderr, "Could not open file: %s", argv[1]);
		return ERROR_FILE_NOT_FOUND;
	}
	std::stack< LN > parse_result;
	try
	{
		parser input_parser(argv[1]);
		parse_result = input_parser.parse();
	} catch (std::bad_alloc& e)
	{
		fprintf(stderr, "Long numbers stack memory exhausted, cannot proceed");
		return ERROR_NOT_ENOUGH_MEMORY;
	} catch (input_file_error& e)
	{
		fprintf(stderr, "Could not open file: %s", e.message);
		return ERROR_FILE_NOT_FOUND;
	} catch (...)
	{
		fprintf(stderr, "Unknown error occurred, cannot proceed");
		return ERROR_UNKNOWN;
	}
	std::ofstream fout;
	fout.open(argv[2]);
	if (!fout)
	{
		fprintf(stderr, "Could not open file: %s", argv[2]);
		return ERROR_FILE_NOT_FOUND;
	}
	while (!parse_result.empty())
	{
		LN res = parse_result.top();
		parse_result.pop();
		fout << res << std::endl;
	}
}