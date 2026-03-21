// Expected exit code: 205
int main(void) {
    long a = 0;
    int b = 0;
    a = 200;
    b = a;
    a = b + 5;
    return a;
}
