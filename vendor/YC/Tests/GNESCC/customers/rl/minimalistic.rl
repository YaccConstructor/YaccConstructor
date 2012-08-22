s0
= module()
  {
    int a = 0;
    int b = 0;

    submodule sm1
    = module()
      [foreign
        language = "Java";
        filename = "sm1.java";
      ];
      [interface
        in  = [a, b];
	out = [a];
      ];
      {
        int a;
        int b;
      }.;

    sm1.a <~ a + 1;
    a <~ b;
  }.