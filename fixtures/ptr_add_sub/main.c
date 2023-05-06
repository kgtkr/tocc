int main() {
    int dummy;
    int *p;
    p = &dummy + 100;
    *p = 454932943;
    *(p + 1) = 19457513;
    return (*p + *(p + 1)) / 4589459;
}
