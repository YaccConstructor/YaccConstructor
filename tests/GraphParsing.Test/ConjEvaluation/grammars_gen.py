def grammars_gen(p_count, b_count):
    f = open('pb_grammar_p'+ str(p_count) + '_b' + str(b_count) + '.yrd', 'w')

    op = []
    cp = []
    ob = []
    cb = []

    for i in range(p_count):
        op.append('OP' + str(i))
        cp.append('CP' + str(i))

    for i in range(b_count):
        ob.append('OB' + str(i))
        cb.append('CB' + str(i))
	
    f.write('[<Start>]\n')
    f.write('s: sp & sb\n')

    f.write('sp: sp sp\n')
    f.write('sp: any_b sp\n')
    f.write('sp: sp any_b\n')
    for i in range(p_count):
        f.write('sp: ' + op[i] + ' sp ' + cp[i] + '\n')
        f.write('sp: ' + op[i] + ' ' + cp[i] + '\n')

    f.write('sb: sb sb\n')
    f.write('sb: any_p sb\n')
    f.write('sb: sb any_p\n')
    for i in range(b_count):
        f.write('sb: ' + ob[i] + ' sb ' + cb[i] + '\n')
        f.write('sb: ' + ob[i] + ' ' + cb[i] + '\n')

    for i in range(p_count):
        f.write('any_p: ' + op[i] + '\n')
        f.write('any_p: ' + cp[i] + '\n')

    for i in range(b_count):
        f.write('any_b: ' + ob[i] + '\n')
        f.write('any_b: ' + cb[i] + '\n')

    f.close()

grammar_args = [(7103,1246),(10400,1360),(21637,3132),(7392,1346),(18597,2857),(6308,1160),(11137,1398),(6519,1228),(6685,1275),(7186,1322),(6217,1152),(455,6),(1132,87),(367,39),(209,25),(715,42),(114,10),(236,28),(78,3),(2795,118),(362,13),(844,173),(2894,330),(12316,1340),(357,8),(166,9)]

for (pc,bc) in grammar_args:
    grammars_gen(pc, bc)


