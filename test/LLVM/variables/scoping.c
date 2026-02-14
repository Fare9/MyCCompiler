// Expected exit code: 3
int main(void) {
    int a = 1;
    {
        int a = 2;
        a = a + 1;
    }
    return a + 2;
}
