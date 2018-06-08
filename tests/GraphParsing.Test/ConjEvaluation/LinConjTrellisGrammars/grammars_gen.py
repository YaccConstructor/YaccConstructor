def grammars_gen(p_count, b_count):
    f = open('pb_grammar_p'+ str(p_count) + '_b' + str(b_count) + '.yrd', 'w')

    any_q = ['q1','q2']

    any_z = ['#']

    for i in range(p_count):
        any_q.append('op' + str(i))
        any_q.append('q1op' + str(i))
        any_z.append('op' + str(i))


    for i in range(b_count):
        any_q.append('ob' + str(i))
        any_q.append('q1ob' + str(i))
        any_z.append('ob' + str(i))


    op = []
    cp = []
    ob = []
    cb = []
    zo = []
    zp = ['#']
    zb = ['#']
    zpb = ['#']

    for i in range(p_count):
        op.append('OP' + str(i))
        cp.append('CP' + str(i))
        zo.append('op' + str(i))
        zp.append('op' + str(i))
        zpb.append('op' + str(i))

    for i in range(b_count):
        ob.append('OB' + str(i))
        cb.append('CB' + str(i))
        zo.append('ob' + str(i))
        zb.append('ob' + str(i))
        zpb.append('ob' + str(i))
	
    f.write('[<Start>]\n')
    for qind in range(len(any_q)):   #(6)
        for zind in range(len(any_z)):
            f.write('q2-#: '+any_q[qind]+'-# q2-'+any_z[zind]+'\n')

    for i in range(p_count):    #(1)-(2)
        f.write('q1-op'+str(i)+': '+op[i]+'\n')
        f.write('op'+str(i)+'-#: '+cp[i]+'\n')

    for i in range(b_count):    #(3)-(4)
        f.write('q1-ob'+str(i)+': '+ob[i]+'\n')
        f.write('ob'+str(i)+'-#: '+cb[i]+'\n')

    for qind in range(len(any_q)):
        for zind in range(len(any_z)):

            for i in range(len(zpb)):  #(5)
                f.write('q1-'+zpb[i]+': '+any_q[qind]+'-'+zpb[i]+' q1-'+any_z[zind]+'\n')

            for i in range(len(zo)):    #(7)
                    f.write('q1-'+zo[i]+': '+any_q[qind]+'-'+zo[i]+' q2-'+any_z[zind]+'\n')

            for i in range(p_count):    #(8)
                f.write('op'+str(i)+'-#: '+any_q[qind]+'-# op'+str(i)+'-'+any_z[zind]+'\n')

            for i in range(p_count):    #(9)
                for j in range(1,len(zb)):
                    f.write('q1op'+str(i)+'-'+zb[j]+': '+any_q[qind]+'-'+zb[j]+' op'+str(i)+'-'+any_z[zind]+'\n')

            for i in range(p_count):    #(10)
                f.write('q2-#: '+any_q[qind]+'-op'+str(i)+' op'+str(i)+'-'+any_z[zind]+'\n')

            for i in range(p_count):    #(11)
                for j in range(len(zb)):
                    f.write('q1op'+str(i)+'-'+zb[j]+': '+any_q[qind]+'-'+zb[j]+' q1op'+str(i)+'-'+any_z[zind]+'\n')

            for i in range(p_count):    #(12)
                f.write('q1-#: '+any_q[qind]+'-op'+str(i)+' q1op'+str(i)+'-'+any_z[zind]+'\n')

            for i in range(b_count):    #(13)
                f.write('ob'+str(i)+'-#: '+any_q[qind]+'-# ob'+str(i)+'-'+any_z[zind]+'\n')

            for i in range(b_count):    #(14)
                for j in range(1,len(zp)):
                    f.write('q1ob'+str(i)+'-'+zp[j]+': '+any_q[qind]+'-'+zp[j]+' ob'+str(i)+'-'+any_z[zind]+'\n')

            for i in range(b_count):    #(15)
                f.write('q2-#: '+any_q[qind]+'-ob'+str(i)+' ob'+str(i)+'-'+any_z[zind]+'\n')

            for i in range(b_count):    #(16)
                for j in range(len(zp)):
                    f.write('q1ob'+str(i)+'-'+zp[j]+': '+any_q[qind]+'-'+zp[j]+' q1ob'+str(i)+'-'+any_z[zind]+'\n')

            for i in range(b_count):    #(17)
                f.write('q1-#: '+any_q[qind]+'-'+ob[i]+' q1ob'+str(i)+'-'+any_z[zind]+'\n')


    #todo rewrite to conjuncts l-term Any & Any r-term



    f.close()

grammar_args = [(78,3)]

"""grammar_args = [(7103,1246),(10400,1360),(21637,3132),(7392,1346),(18597,2857),(6308,1160),(11137,1398),(6519,1228),(6685,1275),(7186,1322),(6217,1152),(455,6),(1132,87),(367,39),(209,25),(715,42),(114,10),(236,28),(78,3),(2795,118),(362,13),(844,173),(2894,330),(12316,1340),(357,8),(166,9)]"""

for (pc,bc) in grammar_args:
    grammars_gen(pc, bc)


