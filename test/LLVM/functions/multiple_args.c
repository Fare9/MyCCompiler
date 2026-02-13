// Expected exit code: 21
int sum3(int a, int b, int c) {
    return a + b + c;
}

int main(void) {
    return sum3(1, sum3(2, 3, 4), sum3(5, 3, 3));
}
