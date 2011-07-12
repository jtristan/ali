
int main(int argc, char *argv[]) {
  int x;
  switch (argc) {
  case 0: x = (int) argv[0][0]; break;
  case 1: x = (int) argv[1][1]; break;
  default: x = (int) argv[2][2]; break;
    }

  return argc;
}
