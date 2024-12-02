#include <stdio.h>
#include <stdlib.h>

#define SIZE 1000

#define gpuErrorCheck(ans, abort) { gpuAssert((ans), __FILE__, __LINE__, abort); }

inline void gpuAssert(cudaError_t code, const char *file, int line, bool abort=true) {
  if(code != cudaSuccess) {
    fprintf(stderr,"assert: %s %s %d\n", cudaGetErrorString(code), file, line);
    if(abort) {
      exit(code);
    }
  }
}


int compare(const void *a, const void *b) {
  int *valA = (int*) a;
  int *valB = (int*) b;
  return *valA - *valB;
}


void parse_input(int* a, int* b) {
    FILE *file = fopen("input.txt", "r");

    char buffer[256];
    int line = 0;
    while (fgets(buffer, sizeof(buffer), file) != NULL && line < 1000) {
        sscanf(buffer, "%d %d", &a[line], &b[line]);
        line++;
    }

    fclose(file);
}

__global__ void diff(int *a, int *b, int *c) {
  // c[blockIdx.x] = a[blockIdx.x] + b[blockIdx.x];
  c[threadIdx.x] = abs(a[threadIdx.x] - b[threadIdx.x]);
}

int main(void) {
  int *a, *b, *c;
  int *d_a, *d_b, *d_c;
  int size = SIZE * sizeof(int);

  gpuErrorCheck(cudaMalloc((void **) &d_a, size), true);
  gpuErrorCheck(cudaMalloc((void **) &d_b, size), true);
  gpuErrorCheck(cudaMalloc((void **) &d_c, size), true);

  a = (int *) malloc(size);
  b = (int *) malloc(size);
  c = (int *) malloc(size);

  parse_input(a, b);

  qsort(a,SIZE,sizeof(int), compare);
  qsort(b,SIZE,sizeof(int), compare);

  // Copy input
  gpuErrorCheck(cudaMemcpy(d_a, a, size, cudaMemcpyHostToDevice), true);
  gpuErrorCheck(cudaMemcpy(d_b, b, size, cudaMemcpyHostToDevice), true);

  // Star1
  print_list(a, SIZE);
  print_list(b, SIZE);

  diff<<<1,SIZE>>>(d_a, d_b, d_c);
  gpuErrorCheck(cudaMemcpy(c, d_c, size, cudaMemcpyDeviceToHost), true);

  print_list(c, SIZE);

  int acc = 0;
  for (int i = 0; i < SIZE; ++i) {
    acc += c[i];
  }

  printf("Star1: %d", acc);

  // Clean
  free(a); free(b); free(c);
  cudaFree(d_a); cudaFree(d_b); cudaFree(d_c);

  return 0;

}
