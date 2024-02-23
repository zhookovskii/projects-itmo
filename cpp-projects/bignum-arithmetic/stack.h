#pragma once

#include <iostream>
#include <malloc.h>

template< class T >
class stack
{
  private:
	void ensureCapacity(unsigned long long newSize)
	{
		while (this->size < newSize)
		{
			if (size > 0)
			{
				T* temp = (T*)realloc(data, (size * 2));
				if (!temp)
				{
					throw std::bad_alloc();
				}
				else
				{
					data = temp;
					size *= 2;
				}
			}
			else
			{
				data = (T*)malloc(sizeof(T));
				size = 1;
			}
		}
	}

  public:
	T* data;
	unsigned long long size = 0;
	unsigned long long write = 0;
	bool isEmpty = true;
	stack(unsigned long long initSize)
	{
		size = initSize;
		write = 0;
		data = (T*)malloc(sizeof(T) * initSize);
		if (!data)
		{
			throw std::bad_alloc();
		}
	}
	stack(const stack& A)
	{
		data = (T*)malloc(A.size);
		size = A.size;
		write = A.write;
		isEmpty = A.isEmpty;
		for (unsigned long long i = 0; i <= A.write; i++)
		{
			data[i] = A.data[i];
		}
	}
	void push_back(T element)
	{
		isEmpty = false;
		if (!size)
		{
			ensureCapacity(1);
			data[write] = element;
		}
		else
		{
			ensureCapacity(write + 2);
			data[++write] = element;
		}
	}
	T pop()
	{
		if (write > 0)
		{
			write--;
		}
		if (write == 0)
		{
			isEmpty = true;
			return data[write];
		}
		return data[write + 1];
	}
	T peek() { return data[write]; }
	~stack()
	{
		if (data)
		{
			free(data);
		}
	}
	stack& operator=(const stack& A)
	{
		if (this == &A)
		{
			return *this;
		}
		free(data);
		data = (T*)malloc(A.size);
		size = A.size;
		write = A.write;
		isEmpty = A.isEmpty;
		for (unsigned long long i = 0; i <= A.write; i++)
		{
			data[i] = A.data[i];
		}
		return *this;
	}
};
