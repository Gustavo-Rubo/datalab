/* 
 * CS:APP Data Lab 
 * 
 * Gustavo Rubo
 * 4584080
 * 
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting an integer by more
     than the word size.

EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implent floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operators (! ~ & ^ | + << >>)
     that you are allowed to use for your implementation of the function. 
     The max operator count is checked by dlc. Note that '=' is not 
     counted; you may use as many of these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies 
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce 
 *      the correct answers.
 */


#endif
/* Copyright (C) 1991-2021 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <https://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses Unicode 10.0.0.  Version 10.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2017, fifth edition, plus
   the following additions from Amendment 1 to the fifth edition:
   - 56 emoji characters
   - 285 hentaigana
   - 3 additional Zanabazar Square characters */
/* 
 * bitAnd - x&y using only ~ and | 
 *   Example: bitAnd(6, 5) = 4
 *   Legal ops: ~ |
 *   Max ops: 8
 *   Rating: 1
 */
int bitAnd(int x, int y) {
  /* boolean arithmetic: A and B = (A' or B')' */ 
  return ~(~x | ~y);
}
/* 
 * getByte - Extract byte n from word x
 *   Bytes numbered from 0 (LSB) to 3 (MSB)
 *   Examples: getByte(0x12345678,1) = 0x56
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 2
 */
int getByte(int x, int n) {
  /* We need to multiply n by 8, as such:
   * 0b00000000 = 0b00000000
   * 0b00000001 = 0b00001000
   * 0b00000010 = 0b00010000
   * 0b00000011 = 0b00011000
   * Multiplication by 8 is equivalent to a bit shift of 3 to the left
   */
  return ((x >> (n << 3)) & 0xff);
}
/* 
 * logicalShift - shift x to the right by n, using a logical shift
 *   Can assume that 0 <= n <= 31
 *   Examples: logicalShift(0x87654321,4) = 0x08765432
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3 
 */
int logicalShift(int x, int n) {
  /* This approach is based on storing the value of the Most Significant Bit,
   * and then making it be zero on x. Then we perform the bitshift,
   * and finally put the MSB back in the place where it should be.
   */

  int minus_n = ~n + 1;            // We'll need this
  int mask = (1 << 31);            // mask for the msb (0x80000000)
  int msb = !!(x & mask);          // store the MSB as a boolean
  x = (x & ~mask) >> n;            // perform bitshift on masked x
  x = x | (msb << (31 + minus_n)); // put the MSB back

  /* Note: one operator could be saved by substituting the last line by:
  x = x | (msb << (32 + ~n));
   * And removing the negative_n declaration.
   * This is not done so that the code is more comprehensible.
   */

  return x;
}
/*
 * bitCount - returns count of number of 1's in word
 *   Examples: bitCount(5) = 2, bitCount(7) = 3
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 40
 *   Rating: 4
 */
int bitCount(int x) {
  /* This is disgusting */

  int count = 0;

  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  count = count + (x & 0x01); x = x >> 1;
  
  return count;
}
/* 
 * bang - Compute !x without using !
 *   Examples: bang(3) = 0, bang(0) = 1
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 4 
 */
int bang(int x) {
  /* This approach consists of dividing the vector in two and
   * storing the value of the OR statement between both halves
   * until we have a vector of size one bit.
   */

  x = (x >> 16) | x;
  x = (x >> 8)  | x;
  x = (x >> 4)  | x;
  x = (x >> 2)  | x;
  x = (x >> 1)  | x;

  return ~x & 0x01;
}
/* 
 * tmin - return minimum two's complement integer 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 4
 *   Rating: 1
 */
int tmin(void) {
  /* The minimum C2 integer is 0x80000000, or 
   * 0b10000000000000000000000000000000.
   */
  return 1 << 31;
}
/* 
 * fitsBits - return 1 if x can be represented as an 
 *  n-bit, two's complement integer.
 *   1 <= n <= 32
 *   Examples: fitsBits(5,3) = 0, fitsBits(-4,3) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int fitsBits(int x, int n) {
  /* x fitting in n bits implies that the
   * first 32 - n + 1 bits are the same.
   * So we shift x by (n - 1) to the right, and compare
   * that to x shifted by 31 to the right (a signal mask)
   */

  int x_shift_n = x >> (n + ~0x0);
  int sign_mask = x >> 31;

  return !(x_shift_n ^ sign_mask);
}
/* 
 * divpwr2 - Compute x/(2^n), for 0 <= n <= 30
 *  Round toward zero
 *   Examples: divpwr2(15,1) = 7, divpwr2(-33,4) = -2
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int divpwr2(int x, int n) {
  /* Used as reference:
   * https://github.com/codinfox/15213-labs/blob/master/datalab/bits.c
   */
  /* We store (x >> n) in x1 (for positive x)
   * and store ((x + (1 << n) - 1) >> n) in x2 (for negative x)
   * Then we select the right answer using a mask.
   */

  int x1 = x >> n;
  int minus_one = ~0;
  int x2 = (x + ((1 << n) + minus_one)) >> n;

  int negative_mask = x >> 31;

  return (x1 & ~negative_mask) | (x2 & negative_mask);
}
/* 
 * negate - return -x 
 *   Example: negate(1) = -1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int negate(int x) {
  /* return the two's complement of the number */
  return ~x + 1;
}
/* 
 * isPositive - return 1 if x > 0, return 0 otherwise 
 *   Example: isPositive(-1) = 0.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 3
 */
int isPositive(int x) {
  /* Get the sign bit and negate it.
   * We also need to make an exception for zero,
   * as it is not considered to be positive, but 
   * has the positive sign bit.
   */

  int mask = (1 << 31);
  int is_zero = !x;
  int minus_is_zero = (~is_zero + 1);
  return !(mask & x) + minus_is_zero;
}
/* 
 * isLessOrEqual - if x <= y  then return 1, else return 0 
 *   Example: isLessOrEqual(4,5) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isLessOrEqual(int x, int y) {
  /* First, we admit that:
   * (x <= y)  <==>  !(x > y)
   * Then, we try to find if (x > y)
   * There are two possibilities for this:
   *   if x is positive (or zero) and y is negative
   *   if they have the same sign and y - x is negative.
   */
  int minus_x = (~x + 1);
  int dif = y + minus_x;

  // x_pos_y_neg: true if x is positive and y is negative
  int x_pos_y_neg = (y & ~x);
  // op: true if x and y have diferent signs (opposites)
  int op = (x ^ y);

  int x_bigger_than_y = ((x_pos_y_neg 
      | (~op & dif)) >> 31) & 1;

  return !x_bigger_than_y;
}
/*
 * ilog2 - return floor(log base 2 of x), where x > 0
 *   Example: ilog2(16) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 90
 *   Rating: 4
 */
int ilog2(int x) {
  /* This problem is equivalent to finding the position
   * of the highest bit in the integer.
   * We will check half of the bits at a time.
   */
  int mask0, mask1, mask2, mask3, mask4;
  int bit0, bit1, bit2, bit3, bit4;

  mask0 = 0xff;
  mask0 = mask0 | mask0 << 8;
  mask1 = 0xff;
  mask1 = mask1 | mask1 << 16;
  mask2 = 0x0f;
  mask2 = mask2 | mask2 << 8;
  mask2 = mask2 | mask2 << 16;
  mask3 = 0x33;
  mask3 = mask3 | mask3 << 8;
  mask3 = mask3 | mask3 << 16;
  mask4 = 0x55;
  mask4 = mask4 | mask4 << 8;
  mask4 = mask4 | mask4 << 16;


  /*
  printf("mask0: %x\n", x & ~mask0);
  printf("mask1: %x\n", x & ~mask1);
  printf("mask2: %x\n", x & ~mask2);
  printf("mask3: %x\n", x & ~mask3);
  printf("mask4: %x\n", x & ~mask4);
  */

  bit4 = !!(x & ~mask0) << 4;
  bit3 = !!(x & ~mask1) << 3;
  bit2 = !!(x & ~mask2) << 2;
  bit1 = !!(x & ~mask3) << 1;
  bit0 = !!(x & ~mask4);

  return bit4 + bit3 + bit2 + bit1 + bit0;
}
/* 
 * float_neg - Return bit-level equivalent of expression -f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned float_neg(unsigned uf) {
  unsigned exp = (uf >> 23) & 0xff;
  unsigned frac = (uf & 0x7fffff);
  int sign_mask = (1 << 31);

  // Special Case: NaN
  if (exp == 0xff && frac != 0) {
    return uf;
  }

  return uf ^ sign_mask;
}
/* 
 * float_i2f - Return bit-level equivalent of expression (float) x
 *   Result is returned as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point values.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_i2f(int x) {
  /* used as reference:
   * https://gist.github.com/yangl1996/81c1355fa9231493ecf1
   */
  /* First, to have the same range of values as integer, we
   * make it so that E = 31 -> exp - bias = 31 -> exp = 158.
   */
  //printf("\nx: %x, %d\n", x, x);

  unsigned sign = x >> 31;
  unsigned exp = 158;
  unsigned frac_mask = 0x007fffff;
  unsigned frac = x & frac_mask;

  // Special case: zero
  if (x == 0) return 0;
  // Special case: 0x80000000
  else if (x == 0x80000000) return 0xcf000000;

  // It's easier to work with only positive integers
  if (x < 0) x = -x;

  // Floating numbers only have a resolution of 23 bits,
  // so we will sacrifice some of the integer values

  return (sign | exp << 23 | frac);
}
/* 
 * float_twice - Return bit-level equivalent of expression 2*f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representation of
 *   single-precision floating point values.
 *   When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_twice(unsigned uf) {
  /* There are two exceptions: zero and NaN
   * There are two main cases:
   *  - when the exponent is equal to zero, we keep
   *     the sign and left shift the rest by one
   *  - else, we increase the exponent by one and
   *     keep the rest
   */

  unsigned sign_mask = 0x80000000;
  unsigned exp_mask  = 0x7f800000;
  unsigned frac_mask = 0x007fffff;

  unsigned sign = uf & sign_mask;
  unsigned exp  = uf & exp_mask;
  unsigned frac = uf & frac_mask;

  // Special Case: NaN
  if (exp == 0x7f800000) return uf;

  // Special case: zero
  if (exp == 0 && frac == 0) return uf;

  if (exp != 0) {
    // This is equivalent of adding one to the exponent
    return uf + 0x00800000;
  }
  else {
    return sign | (uf << 1);
  }
}