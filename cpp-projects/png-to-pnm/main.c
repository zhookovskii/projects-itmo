#include <malloc.h>
#include <math.h>
#include <stdio.h>
#include <zlib.h>

#include "return_codes.h"

unsigned int fromHexToDec(const unsigned char* buffer, int buffer_size)
{
    unsigned int sum = 0;
    for (int i = 0; i < buffer_size; i++)
    {
        int curr = buffer[i];
        sum = sum + curr * pow(16, (buffer_size - i - 1) * 2);
    }
    return sum;
}

unsigned char* createBuffer(int size)
{
    unsigned char* buffer = malloc(sizeof(unsigned char) * size);
    return buffer;
}

int readPngSignature(FILE* input)
{
    unsigned char* buffer = createBuffer(8);
    if (!buffer)
    {
        printf("Not enough memory resources are available to process this command");
        return ERROR_NOT_ENOUGH_MEMORY;
    }
    fread(buffer, 1, 8, input);
    unsigned char correct[8] = { 137, 80, 78, 71, 13, 10, 26, 10 };	   //	correct PNG signature
    for (int i = 0; i < 8; i++)
    {
        if (buffer[i] != correct[i])
        {
            printf("The input file is whether invalid or not PNG");
            fclose(input);
            return ERROR_INVALID_DATA;
        }
    }
    free(buffer);
    return ERROR_SUCCESS;
}

int readIhdrChunk(FILE* input, unsigned int* image_width, unsigned int* image_height, unsigned int* image_color_type)
{
    unsigned char* buffer = createBuffer(8);
    if (!buffer)
    {
        printf("Not enough memory resources are available to proceed this command");
        return ERROR_NOT_ENOUGH_MEMORY;
    }
    fread(buffer, 1, 8, input);									   //	reading IHDR chunk length and signature
    unsigned char correct[8] = { 0, 0, 0, 13, 73, 72, 68, 82 };	   //	correct length and name of IHDR
    for (int i = 0; i < 8; i++)
    {
        if (buffer[i] != correct[i])
        {
            printf("The input file is whether invalid or not PNG");
            return ERROR_INVALID_DATA;
        }
    }
    fread(buffer, 1, 4, input);	   //	reading width
    unsigned int width = fromHexToDec(buffer, 4);
    if (!width)
    {
        printf("Cannot decode image with zero width");
        return ERROR_INVALID_DATA;
    }
    *image_width = width;
    fread(buffer, 1, 4, input);	   //	reading heigth
    unsigned int height = fromHexToDec(buffer, 4);
    if (!height)
    {
        printf("Cannot decode image with zero height");
        return ERROR_INVALID_DATA;
    }
    *image_height = height;
    fread(buffer, 1, 1, input);	   //	reading bit depth
    unsigned int bit_depth = fromHexToDec(buffer, 1);
    if (bit_depth != 8)
    {
        printf("Cannot proceed with given bit depth");
        return ERROR_INVALID_DATA;
    }
    fread(buffer, 1, 1, input);	   //	reading color type
    unsigned int color_type = fromHexToDec(buffer, 1);
    if (color_type != 0 && color_type != 2)
    {
        printf("Cannot proceed with given color type");
        return ERROR_INVALID_DATA;
    }
    *image_color_type = color_type;
    fread(buffer, 1, 1, input);	   //	reading compression method
    unsigned int compression_method = fromHexToDec(buffer, 1);
    if (compression_method)
    {
        printf("Compression method differs from deflate, cannot proceed");
        return ERROR_INVALID_DATA;
    }
    fread(buffer, 1, 1, input);	   //	reading filtration method
    unsigned int filtration_method = fromHexToDec(buffer, 1);
    if (filtration_method)
    {
        printf("Filtration method differs from deflate, cannot proceed");
        return ERROR_INVALID_DATA;
    }
    fread(buffer, 1, 1, input);	   //	reading interlace method
    unsigned int interlace_method = fromHexToDec(buffer, 1);
    if (interlace_method && interlace_method != 1)
    {
        printf("Unknown interlace method, cannot proceed");
        return ERROR_INVALID_DATA;
    }
    fread(buffer, 1, 4, input);	   // reading CRC
    free(buffer);
    return ERROR_SUCCESS;
}

int readIdatChunk(FILE* input, unsigned int length, unsigned char** data, unsigned int* data_size)
{
    unsigned int new_size = *(data_size) + length;
    unsigned char* new_data = realloc(*data, sizeof(unsigned char) * new_size);
    if (new_data)
    {
        *data = new_data;
    }
    else
    {
        printf("Not enough memory resources are available to proceed this command");
        return ERROR_NOT_ENOUGH_MEMORY;
    }
    unsigned char* buffer = createBuffer(length + 4);
    if (!buffer)
    {
        printf("Not enough memory resources are available to proceed this command");
        return ERROR_NOT_ENOUGH_MEMORY;
    }
    fread(buffer, 1, length + 4, input);
    for (int i = *(data_size); i < new_size; i++)
    {
        (*data)[i] = buffer[i - *(data_size)];
    }
    free(buffer);
    *(data_size) = new_size;
    return ERROR_SUCCESS;
}

int readChunks(FILE* input, unsigned char** imageData, unsigned int* data_size)
{
    unsigned char idatName[4] = { 73, 68, 65, 84 };
    unsigned char iendName[4] = { 73, 69, 78, 68 };
    unsigned char* buffer = createBuffer(4);
    if (!buffer)
    {
        printf("Not enough memory resources are available to proceed this command");
        return ERROR_NOT_ENOUGH_MEMORY;
    }
    fread(buffer, 1, 4, input);	   //	reading chunk length
    unsigned int chunkLength = fromHexToDec(buffer, 4);
    fread(buffer, 1, 4, input);	   //	reading chunk name
    int isIdat = 1;
    int isIend = 1;
    for (int i = 0; i < 4; i++)
    {
        if (!isIdat && !isIend)
        {
            break;
        }
        if (buffer[i] != idatName[i])
        {
            isIdat = 0;
        }
        if (buffer[i] != iendName[i])
        {
            isIend = 0;
        }
    }
    free(buffer);
    if (isIend)
    {
        return 101;	   //	the end of file is reached
    }
    if (isIdat)
    {
        int readIdat = readIdatChunk(input, chunkLength, imageData, data_size);
        if (readIdat)
        {
            return readIdat;
        }
    }
    else
    {
        buffer = createBuffer(chunkLength + 4);	   //	reading chunk info and crc
        if (!buffer)
        {
            printf("Not enough memory resources are available to proceed this command");
            return ERROR_NOT_ENOUGH_MEMORY;
        }
        fread(buffer, 1, chunkLength + 4, input);
        free(buffer);
    }
    return ERROR_SUCCESS;
}

int main(int argc, char** argv)
{
    if (argc != 3)
    {
        printf("Incorrect input, try: \"lab2 input.png output.pnm\" ");
        return ERROR_INVALID_PARAMETER;
    }
    FILE* in = fopen(argv[1], "rb");
    if (!in)
    {
        printf("The system cannot find the file specified: %s", argv[1]);
        return ERROR_FILE_NOT_FOUND;
    }
    int pngSignature = readPngSignature(in);
    if (pngSignature)
    {
        fclose(in);
        return pngSignature;
    }
    unsigned int image_width;
    unsigned int image_height;
    unsigned int image_color_type;
    int ihdrChunk = readIhdrChunk(in, &image_width, &image_height, &image_color_type);
    if (ihdrChunk)
    {
        fclose(in);
        return ihdrChunk;
    }
    unsigned char* imageData = NULL;
    unsigned int data_size = 0;
    int chunks = readChunks(in, &imageData, &data_size);
    while (!chunks)
    {
        chunks = readChunks(in, &imageData, &data_size);
    }
    if (chunks != 101)	  //	return code 101 means we reached IEND chunk successfully
    {
        free(imageData);
        fclose(in);
        return chunks;
    }
    FILE* out = fopen(argv[2], "wb");
    if (!out)
    {
        printf("Cannot open file: %s", argv[2]);
        fclose(in);
        free(imageData);
        return ERROR_FILE_NOT_FOUND;
    }
    uLongf uncompressData = (image_color_type + 1)*image_width*image_height + image_height;
    unsigned char* imageUncompress = malloc(sizeof(unsigned char) * uncompressData);
    int res = uncompress(imageUncompress, &uncompressData, imageData, data_size + 1);
    printf("%i", res);
    return ERROR_SUCCESS;
}