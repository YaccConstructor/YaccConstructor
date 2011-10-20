s0 <- module()
      {
        e1 <- event
              [on 
                condition : (a = 10) & (b < 5);
                calls     : [f1];
              ]; 
     };