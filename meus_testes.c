#include <stdio.h>

void printBits(int num) {
	int size = sizeof(num);
	int maxPower = 1 << (size*8-1);
	for(int i = 0; i < size*8; ++i){
	    // print last bit and shift left.
	    printf("%u",num&maxPower ? 1 : 0);
	    num = num<<1;
	}
	printf("\n");
}

int main () {

  // bitAnd
  /*	
  printf("%d\n",  0b00000000);
  printf("%d\n", !0b00000000);
  printf("%d\n",  0b00000010);
  printf("%d\n", !0b00000010);


  printf("\n");
  printf("%d\n", ~0b00000000);
  printf("%d\n", ~0b00000001);
  printf("%d\n", ~0b00000010);
  printf("%d\n", ~0b00000011);

  printBits(~0b00000000);
  printBits(~0b00000001);
  printBits(~0b00000010);
  printBits(~0b00000011);
  */

  // logicalShift
  /*
  printf("%x\n", 0x87654321);
  printf("%x\n", 0x87654321 >> 4);
  printBits(0x87654321);
  printBits(0x87654321 >> 4);

  short n = 1;
  int mask = (1 << 31) >> n;
  int x = 0x80000000;
  printBits(x);
  printBits(mask);
  printf("%x\n", x);
  printf("%x\n", (x >> n) & ~mask);
  */
  
  // bitCount
  /*
  printf("%d\n",  0b0000);
  printf("%d\n", !0b0000);
  printf("%d\n",  0b0001);
  printf("%d\n", !0b0001);
  printf("%d\n",  0b0010);
  printf("%d\n", !0b0010);
  */

  // fitsBits
  int x = 0x80000000;
  int mask = (1 << 31); // equals to 0x8000000
  int msb = x & mask;   // 0x80000000 if negative, 0x0 if positive
  int mask2 = msb >> 31; // take advantage of the arithimetic shift
  printBits(mask2);
  printf("%32x\n", mask2);
  x = x ^ mask2;      // modulus of the number
  x = x & ~mask; // number with no sign
  x = x + 1;

  printBits(x);
  printf("%x\n", x);
  x = (x >> 1) & ~mask;
  //printf("%d", !(x >> (32)));

}