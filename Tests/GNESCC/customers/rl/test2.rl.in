s0 <- module()
      {
        a <- 0;
        b <- 0;
        c <- 0;
        d <- 0;

        sm1 <- module()
               [foreign
                 language : "Java";
                 filename : "sm1.java";
               ]
               [interface
                 in  : [a, b];
                 out : [a];
               ];

        f1 <- fun(x, y, z)
              {
                b <- x + y + z;
              };

        h1 <- handler
              {
                c <- new(a);
              };   

        e1 <- event
              [on 
                condition : (a = 10) & (b < 5);
                calls     : [f1];
              ],
           <@ h1,
           <@ handler
              {
                c <- 10;
              };

        e2 <- !(d = 5, [f1]);
        e3 <- event(d = 4, []);
        h2 <- #{d <- 0; a <- 8;};

        !(a = 5, [h1]) <@ #{c <- 5;};

        sm1.a <~ a + 1;
        a <~ f(b, c, d);
      };