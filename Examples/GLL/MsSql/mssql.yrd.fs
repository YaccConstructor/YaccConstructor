(*
0 dec_num -> yard_opt_1 DEC_NUMBER
1 yard_opt_1 -> OP_MINUS
2 yard_opt_1 -> 
3 sql_value -> KW_NULL
4 sql_value -> dec_num
5 sql_value -> STRING_CONST
6 ident -> yard_rule_brace_opt_1
7 yard_rule_brace_opt_1 -> LBRACKET yard_exp_brackets_94 RBRACKET
8 yard_rule_brace_opt_1 -> yard_exp_brackets_93
9 full_ident -> yard_rule_not_empty_list_2
10 yard_rule_not_empty_list_2 -> ident yard_rule_yard_many_4_3
11 yard_rule_yard_many_4_3 -> yard_exp_brackets_95 yard_rule_yard_many_4_3
12 yard_rule_yard_many_4_3 -> 
13 yard_exp_brackets_93 -> KW_COUNT
14 yard_exp_brackets_93 -> KW_IGNORE_DUP_KEY
15 yard_exp_brackets_93 -> KW_OWNER
16 yard_exp_brackets_93 -> KW_STATE
17 yard_exp_brackets_93 -> KW_CLASS
18 yard_exp_brackets_93 -> KW_TYPE
19 yard_exp_brackets_93 -> IDENT
20 yard_exp_brackets_94 -> KW_COUNT
21 yard_exp_brackets_94 -> KW_IGNORE_DUP_KEY
22 yard_exp_brackets_94 -> KW_OWNER
23 yard_exp_brackets_94 -> KW_STATE
24 yard_exp_brackets_94 -> KW_CLASS
25 yard_exp_brackets_94 -> KW_TYPE
26 yard_exp_brackets_94 -> IDENT
27 yard_exp_brackets_95 -> DOT ident
28 root_rule -> yard_many_5
29 yard_start_rule -> root_rule
30 yard_opt_2 -> KW_GO
31 yard_opt_2 -> 
32 yard_many_5 -> yard_exp_brackets_96 yard_many_5
33 yard_many_5 -> 
34 batch_body -> insert
35 batch_body -> create_table
36 batch_body -> drop_procedure
37 batch_body -> if_stmt
38 batch_body -> rollback_transaction
39 batch_body -> case
40 batch_body -> declare
41 batch_body -> commit_transaction
42 batch_body -> begin_transaction
43 batch_body -> KW_USE LBRACKET KW_MDW_CONTROL RBRACKET
44 batch_body -> set_stmnt
45 batch_body -> create_proc
46 rollback_transaction -> KW_ROLLBACK yard_opt_3 yard_opt_4 yard_opt_5
47 yard_opt_3 -> yard_exp_brackets_97
48 yard_opt_3 -> 
49 yard_opt_4 -> yard_exp_brackets_98
50 yard_opt_4 -> 
51 yard_opt_5 -> SEMI
52 yard_opt_5 -> 
53 set_stmnt -> set_localvar
54 set_stmnt -> KW_SET yard_exp_brackets_99 yard_exp_brackets_100
55 set_localvar -> yard_exp_brackets_102
56 set_localvar -> KW_SET yard_exp_brackets_101
57 yard_rule_comma_list_4 -> yard_rule_list_5
58 yard_rule_list_5 -> proc_formal_param yard_rule_yard_many_3_6
59 yard_rule_list_5 -> 
60 yard_rule_yard_many_3_6 -> yard_exp_brackets_103 yard_rule_yard_many_3_6
61 yard_rule_yard_many_3_6 -> 
62 yard_opt_6 -> yard_exp_brackets_104
63 yard_opt_6 -> 
64 yard_opt_7 -> yard_exp_brackets_105
65 yard_opt_7 -> 
66 yard_opt_8 -> yard_exp_brackets_106
67 yard_opt_8 -> 
68 yard_opt_9 -> yard_exp_brackets_107
69 yard_opt_9 -> 
70 yard_opt_10 -> KW_TYPE_WARNING
71 yard_opt_10 -> 
72 yard_opt_12 -> yard_exp_brackets_108
73 yard_opt_12 -> 
74 yard_rule_comma_list_7 -> yard_rule_list_8
75 yard_rule_list_8 -> ident yard_rule_yard_many_3_9
76 yard_rule_list_8 -> 
77 yard_rule_yard_many_3_9 -> yard_exp_brackets_109 yard_rule_yard_many_3_9
78 yard_rule_yard_many_3_9 -> 
79 yard_opt_11 -> yard_exp_brackets_110
80 yard_opt_11 -> 
81 drop_procedure -> KW_DROP yard_exp_brackets_111 yard_some_1
82 yard_opt_13 -> yard_exp_brackets_112
83 yard_opt_13 -> 
84 yard_some_1 -> yard_exp_brackets_114 yard_some_1
85 yard_some_1 -> yard_exp_brackets_113
86 proc_formal_param -> LOCALVAR yard_opt_14 yard_opt_15 yard_opt_16 yard_opt_17 yard_opt_18 yard_opt_19
87 yard_opt_14 -> yard_exp_brackets_115
88 yard_opt_14 -> 
89 yard_opt_15 -> sql_datatype
90 yard_opt_15 -> 
91 yard_opt_16 -> KW_VARYING
92 yard_opt_16 -> 
93 yard_opt_17 -> yard_exp_brackets_116
94 yard_opt_17 -> 
95 yard_opt_18 -> yard_exp_brackets_117
96 yard_opt_18 -> 
97 yard_opt_19 -> KW_READONLY
98 yard_opt_19 -> 
99 create_proc -> KW_CREATE yard_exp_brackets_118 yard_opt_20 ident yard_opt_21 yard_rule_brace_opt_10 yard_opt_22 yard_opt_23 KW_AS yard_many_6 yard_opt_25
100 yard_rule_brace_opt_10 -> LPAREN yard_rule_comma_list_4 RPAREN
101 yard_rule_brace_opt_10 -> yard_rule_comma_list_4
102 yard_opt_20 -> yard_exp_brackets_119
103 yard_opt_20 -> 
104 yard_opt_21 -> yard_exp_brackets_120
105 yard_opt_21 -> 
106 yard_some_2 -> execute_as yard_some_2
107 yard_some_2 -> execute_as
108 yard_opt_22 -> yard_exp_brackets_121
109 yard_opt_22 -> 
110 yard_opt_23 -> yard_exp_brackets_122
111 yard_opt_23 -> 
112 yard_opt_24 -> SEMI
113 yard_opt_24 -> 
114 yard_many_6 -> yard_exp_brackets_123 yard_many_6
115 yard_many_6 -> 
116 yard_opt_25 -> SEMI
117 yard_opt_25 -> 
118 proc_body_stmnt -> print_stmt
119 proc_body_stmnt -> fetch_stmnt
120 proc_body_stmnt -> insert
121 proc_body_stmnt -> create_table
122 proc_body_stmnt -> while_stmt
123 proc_body_stmnt -> drop_procedure
124 proc_body_stmnt -> rollback_transaction
125 proc_body_stmnt -> commit_transaction
126 proc_body_stmnt -> begin_transaction
127 proc_body_stmnt -> stmt_block
128 proc_body_stmnt -> raiserror_stmnt
129 proc_body_stmnt -> if_stmt
130 proc_body_stmnt -> KW_RETURN sql_expr
131 proc_body_stmnt -> declare
132 proc_body_stmnt -> execute_stmnt
133 proc_body_stmnt -> set_stmnt
134 proc_body_stmnt -> select_stmnt
135 print_stmt -> KW_PRINT sql_expr yard_opt_29
136 yard_opt_29 -> SEMI
137 yard_opt_29 -> 
138 while_stmt -> KW_WHILE sql_expr proc_body_stmnt
139 stmt_block -> KW_BEGIN yard_many_7 KW_END
140 yard_opt_30 -> SEMI
141 yard_opt_30 -> 
142 yard_many_7 -> yard_exp_brackets_124 yard_many_7
143 yard_many_7 -> 
144 if_stmt -> KW_IF sql_expr proc_body_stmnt yard_opt_31
145 yard_opt_31 -> yard_exp_brackets_125
146 yard_opt_31 -> 
147 raiserror_stmnt -> KW_RAISERROR LPAREN yard_exp_brackets_126 yard_exp_brackets_127 yard_opt_32 RPAREN yard_opt_33
148 yard_opt_32 -> yard_exp_brackets_128
149 yard_opt_32 -> 
150 yard_rule_comma_list_11 -> yard_rule_list_12
151 yard_rule_list_12 -> sql_expr yard_rule_yard_many_3_13
152 yard_rule_list_12 -> 
153 yard_rule_yard_many_3_13 -> yard_exp_brackets_129 yard_rule_yard_many_3_13
154 yard_rule_yard_many_3_13 -> 
155 yard_opt_33 -> yard_exp_brackets_130
156 yard_opt_33 -> 
157 yard_rule_comma_list_14 -> yard_rule_list_15
158 yard_rule_list_15 -> yard_exp_brackets_131 yard_rule_yard_many_3_16
159 yard_rule_list_15 -> 
160 yard_rule_yard_many_3_16 -> yard_exp_brackets_132 yard_rule_yard_many_3_16
161 yard_rule_yard_many_3_16 -> 
162 declare -> KW_DECLARE yard_rule_comma_list_17 yard_opt_39
163 yard_rule_comma_list_17 -> yard_rule_list_18
164 yard_rule_list_18 -> yard_exp_brackets_133 yard_rule_yard_many_3_19
165 yard_rule_list_18 -> 
166 yard_rule_yard_many_3_19 -> yard_exp_brackets_134 yard_rule_yard_many_3_19
167 yard_rule_yard_many_3_19 -> 
168 yard_opt_34 -> KW_AS
169 yard_opt_34 -> 
170 yard_opt_35 -> KW_LOCAL
171 yard_opt_35 -> 
172 yard_opt_36 -> KW_STATIC
173 yard_opt_36 -> 
174 yard_opt_37 -> KW_FOR
175 yard_opt_37 -> 
176 yard_opt_38 -> KW_AS
177 yard_opt_38 -> 
178 yard_opt_39 -> SEMI
179 yard_opt_39 -> 
180 table_type_definition -> KW_TABLE LPAREN yard_many_8 RPAREN
181 yard_many_8 -> yard_exp_brackets_135 yard_many_8
182 yard_many_8 -> 
183 column_definition -> ident yard_exp_brackets_136 yard_opt_40 yard_opt_41 yard_opt_44 yard_opt_45
184 yard_opt_40 -> yard_exp_brackets_137
185 yard_opt_40 -> 
186 yard_opt_42 -> yard_exp_brackets_138
187 yard_opt_42 -> 
188 yard_opt_43 -> yard_exp_brackets_139
189 yard_opt_43 -> 
190 yard_opt_41 -> yard_exp_brackets_140
191 yard_opt_41 -> 
192 yard_opt_44 -> KW_ROWGUIDCOL
193 yard_opt_44 -> 
194 yard_opt_45 -> column_constraint
195 yard_opt_45 -> 
196 column_constraint -> yard_exp_brackets_141
197 yard_opt_46 -> yard_exp_brackets_142
198 yard_opt_46 -> 
199 yard_opt_47 -> yard_exp_brackets_143
200 yard_opt_47 -> 
201 table_constraint -> yard_exp_brackets_145
202 table_constraint -> yard_exp_brackets_144
203 select_stmnt -> yard_exp_brackets_146 yard_opt_55
204 yard_opt_48 -> yard_exp_brackets_147
205 yard_opt_48 -> 
206 yard_rule_comma_list_20 -> yard_rule_list_21
207 yard_rule_list_21 -> common_table_expression yard_rule_yard_many_3_22
208 yard_rule_list_21 -> 
209 yard_rule_yard_many_3_22 -> yard_exp_brackets_148 yard_rule_yard_many_3_22
210 yard_rule_yard_many_3_22 -> 
211 yard_opt_50 -> yard_exp_brackets_149
212 yard_opt_50 -> 
213 yard_opt_49 -> yard_exp_brackets_150
214 yard_opt_49 -> 
215 yard_rule_comma_list_23 -> yard_rule_list_24
216 yard_rule_list_24 -> yard_exp_brackets_151 yard_rule_yard_many_3_25
217 yard_rule_list_24 -> 
218 yard_rule_yard_many_3_25 -> yard_exp_brackets_152 yard_rule_yard_many_3_25
219 yard_rule_yard_many_3_25 -> 
220 yard_opt_52 -> yard_exp_brackets_153
221 yard_opt_52 -> 
222 yard_opt_51 -> yard_exp_brackets_154
223 yard_opt_51 -> 
224 yard_rule_comma_list_26 -> yard_rule_list_27
225 yard_rule_list_27 -> yard_exp_brackets_155 yard_rule_yard_many_3_28
226 yard_rule_list_27 -> 
227 yard_rule_yard_many_3_28 -> yard_exp_brackets_156 yard_rule_yard_many_3_28
228 yard_rule_yard_many_3_28 -> 
229 yard_opt_53 -> for
230 yard_opt_53 -> 
231 yard_opt_54 -> yard_exp_brackets_157
232 yard_opt_54 -> 
233 yard_rule_comma_list_29 -> yard_rule_list_30
234 yard_rule_list_30 -> query_hint yard_rule_yard_many_3_31
235 yard_rule_list_30 -> 
236 yard_rule_yard_many_3_31 -> yard_exp_brackets_158 yard_rule_yard_many_3_31
237 yard_rule_yard_many_3_31 -> 
238 yard_opt_55 -> SEMI
239 yard_opt_55 -> 
240 for -> yard_opt_56
241 yard_opt_56 -> yard_exp_brackets_159
242 yard_opt_56 -> 
243 xml -> KW_XML yard_exp_brackets_160
244 yard_opt_57 -> yard_exp_brackets_161
245 yard_opt_57 -> 
246 yard_opt_60 -> yard_exp_brackets_162
247 yard_opt_60 -> 
248 yard_opt_59 -> yard_exp_brackets_163
249 yard_opt_59 -> 
250 yard_opt_62 -> yard_exp_brackets_164
251 yard_opt_62 -> 
252 yard_opt_61 -> yard_exp_brackets_165
253 yard_opt_61 -> 
254 yard_opt_58 -> yard_exp_brackets_166
255 yard_opt_58 -> 
256 yard_opt_64 -> yard_exp_brackets_167
257 yard_opt_64 -> 
258 yard_opt_63 -> yard_exp_brackets_168
259 yard_opt_63 -> 
260 yard_opt_65 -> yard_exp_brackets_169
261 yard_opt_65 -> 
262 yard_opt_68 -> yard_exp_brackets_170
263 yard_opt_68 -> 
264 yard_opt_67 -> yard_exp_brackets_171
265 yard_opt_67 -> 
266 yard_opt_66 -> yard_exp_brackets_172
267 yard_opt_66 -> 
268 common_directives -> yard_opt_69 yard_opt_70 yard_opt_71
269 yard_opt_69 -> yard_exp_brackets_173
270 yard_opt_69 -> 
271 yard_opt_70 -> yard_exp_brackets_174
272 yard_opt_70 -> 
273 yard_opt_72 -> yard_exp_brackets_175
274 yard_opt_72 -> 
275 yard_opt_71 -> yard_exp_brackets_176
276 yard_opt_71 -> 
277 query_hint -> yard_exp_brackets_177
278 yard_rule_comma_list_32 -> yard_rule_list_33
279 yard_rule_list_33 -> yard_exp_brackets_178 yard_rule_yard_many_3_34
280 yard_rule_list_33 -> 
281 yard_rule_yard_many_3_34 -> yard_exp_brackets_179 yard_rule_yard_many_3_34
282 yard_rule_yard_many_3_34 -> 
283 common_table_expression -> yard_exp_brackets_180 yard_opt_73 KW_AS LPAREN select_stmnt RPAREN yard_opt_74
284 yard_opt_73 -> yard_exp_brackets_181
285 yard_opt_73 -> 
286 yard_opt_74 -> SEMI
287 yard_opt_74 -> 
288 query_expression -> yard_exp_brackets_182 yard_opt_75
289 yard_opt_76 -> KW_ALL
290 yard_opt_76 -> 
291 yard_opt_75 -> yard_exp_brackets_183
292 yard_opt_75 -> 
293 yard_rule_comma_list_35 -> yard_rule_list_36
294 yard_rule_list_36 -> yard_exp_brackets_184 yard_rule_yard_many_3_37
295 yard_rule_list_36 -> 
296 yard_rule_yard_many_3_37 -> yard_exp_brackets_185 yard_rule_yard_many_3_37
297 yard_rule_yard_many_3_37 -> 
298 query_specification -> KW_SELECT yard_opt_77 yard_opt_78 select_list yard_opt_81 yard_opt_82 yard_opt_83 yard_opt_84 yard_opt_87
299 yard_opt_77 -> yard_exp_brackets_186
300 yard_opt_77 -> 
301 yard_opt_79 -> KW_PERCENT
302 yard_opt_79 -> 
303 yard_opt_80 -> yard_exp_brackets_187
304 yard_opt_80 -> 
305 yard_opt_78 -> yard_exp_brackets_188
306 yard_opt_78 -> 
307 yard_opt_81 -> yard_exp_brackets_189
308 yard_opt_81 -> 
309 yard_opt_82 -> from
310 yard_opt_82 -> 
311 yard_opt_83 -> yard_exp_brackets_190
312 yard_opt_83 -> 
313 yard_opt_85 -> KW_ALL
314 yard_opt_85 -> 
315 yard_opt_86 -> yard_exp_brackets_191
316 yard_opt_86 -> 
317 yard_opt_84 -> yard_exp_brackets_192
318 yard_opt_84 -> 
319 yard_opt_87 -> yard_exp_brackets_193
320 yard_opt_87 -> 
321 select_list -> yard_rule_comma_list_38
322 yard_rule_comma_list_38 -> yard_rule_list_39
323 yard_rule_list_39 -> yard_exp_brackets_194 yard_rule_yard_many_3_40
324 yard_rule_list_39 -> 
325 yard_rule_yard_many_3_40 -> yard_exp_brackets_195 yard_rule_yard_many_3_40
326 yard_rule_yard_many_3_40 -> 
327 yard_opt_88 -> yard_exp_brackets_196
328 yard_opt_88 -> 
329 yard_opt_89 -> yard_exp_brackets_197
330 yard_opt_89 -> 
331 yard_opt_91 -> KW_AS
332 yard_opt_91 -> 
333 yard_opt_90 -> yard_exp_brackets_198
334 yard_opt_90 -> 
335 scalar_function -> KW_SUM LPAREN yard_opt_92 sql_expr RPAREN
336 scalar_function -> KW_LOWER sql_expr
337 yard_opt_92 -> yard_exp_brackets_199
338 yard_opt_92 -> 
339 aggregate_windowed_function -> KW_OVER LPAREN yard_rule_comma_list_41 order_by_clause RPAREN
340 yard_rule_comma_list_41 -> yard_rule_list_42
341 yard_rule_list_42 -> yard_opt_93 yard_rule_yard_many_3_43
342 yard_rule_list_42 -> 
343 yard_rule_yard_many_3_43 -> yard_exp_brackets_200 yard_rule_yard_many_3_43
344 yard_rule_yard_many_3_43 -> 
345 yard_opt_93 -> yard_exp_brackets_201
346 yard_opt_93 -> 
347 ranking_windowed_function -> yard_exp_brackets_202 LPAREN yard_opt_94 RPAREN KW_OVER LPAREN yard_opt_95 order_by_clause RPAREN
348 yard_opt_94 -> sql_expr
349 yard_opt_94 -> 
350 yard_many_9 -> yard_exp_brackets_203 yard_many_9
351 yard_many_9 -> 
352 yard_opt_95 -> yard_many_9
353 yard_opt_95 -> 
354 order_by_clause -> yard_opt_96
355 yard_opt_97 -> yard_exp_brackets_204
356 yard_opt_97 -> 
357 yard_opt_96 -> yard_exp_brackets_205
358 yard_opt_96 -> 
359 yard_rule_comma_list_44 -> yard_rule_list_45
360 yard_rule_list_45 -> yard_exp_brackets_206 yard_rule_yard_many_3_46
361 yard_rule_list_45 -> 
362 yard_rule_yard_many_3_46 -> yard_exp_brackets_207 yard_rule_yard_many_3_46
363 yard_rule_yard_many_3_46 -> 
364 search_condition -> yard_exp_brackets_208 yard_many_10
365 yard_opt_98 -> KW_NOT
366 yard_opt_98 -> 
367 yard_opt_99 -> KW_NOT
368 yard_opt_99 -> 
369 yard_many_10 -> yard_exp_brackets_209 yard_many_10
370 yard_many_10 -> 
371 predicate -> IDENT yard_opt_103 yard_opt_104 KW_IN LPAREN yard_exp_brackets_211 RPAREN
372 predicate -> KW_EXISTS LPAREN query_expression RPAREN
373 predicate -> sql_expr binop yard_exp_brackets_210 LPAREN query_expression RPAREN
374 predicate -> sql_expr KW_IS yard_opt_102 KW_NULL
375 predicate -> sql_expr yard_opt_101 KW_BETWEEN sql_expr KW_AND sql_expr
376 predicate -> sql_expr yard_opt_100
377 yard_opt_100 -> yard_exp_brackets_212
378 yard_opt_100 -> 
379 yard_opt_101 -> KW_NOT
380 yard_opt_101 -> 
381 yard_opt_102 -> KW_NOT
382 yard_opt_102 -> 
383 yard_opt_103 -> yard_exp_brackets_213
384 yard_opt_103 -> 
385 yard_opt_104 -> yard_exp_brackets_214
386 yard_opt_104 -> 
387 from -> yard_opt_105
388 yard_opt_105 -> yard_exp_brackets_215
389 yard_opt_105 -> 
390 yard_rule_comma_list_47 -> yard_rule_list_48
391 yard_rule_list_48 -> table_source yard_rule_yard_many_3_49
392 yard_rule_list_48 -> 
393 yard_rule_yard_many_3_49 -> yard_exp_brackets_216 yard_rule_yard_many_3_49
394 yard_rule_yard_many_3_49 -> 
395 table_source -> LOCALVAR DOT ident LPAREN yard_rule_comma_list_11 RPAREN yard_opt_119 yard_opt_120 yard_lr_table_source
396 table_source -> LOCALVAR yard_opt_117 yard_lr_table_source
397 table_source -> LPAREN joined_table RPAREN yard_lr_table_source
398 table_source -> ident yard_opt_115 ident yard_opt_116 yard_lr_table_source
399 table_source -> yard_exp_brackets_217 yard_lr_table_source
400 table_source -> full_ident yard_opt_106 full_ident yard_opt_107 yard_opt_108 yard_lr_table_source
401 yard_lr_table_source -> KW_UNPIVOT unpivot_clause full_ident yard_lr_table_source
402 yard_lr_table_source -> KW_PIVOT pivot_clause full_ident yard_lr_table_source
403 yard_lr_table_source -> KW_CROSS KW_JOIN table_source yard_lr_table_source
404 yard_lr_table_source -> join_type table_source KW_ON search_condition yard_lr_table_source
405 yard_lr_table_source -> 
406 yard_opt_106 -> KW_AS
407 yard_opt_106 -> 
408 yard_opt_107 -> tablesample_clause
409 yard_opt_107 -> 
410 yard_many_11 -> COMMA yard_many_11
411 yard_many_11 -> 
412 yard_opt_108 -> yard_exp_brackets_219
413 yard_opt_108 -> 
414 yard_opt_110 -> KW_AS
415 yard_opt_110 -> 
416 yard_opt_109 -> yard_exp_brackets_220
417 yard_opt_109 -> 
418 yard_opt_111 -> yard_exp_brackets_221
419 yard_opt_111 -> 
420 yard_opt_113 -> KW_AS
421 yard_opt_113 -> 
422 yard_opt_112 -> yard_exp_brackets_222
423 yard_opt_112 -> 
424 yard_opt_114 -> yard_exp_brackets_223
425 yard_opt_114 -> 
426 yard_opt_115 -> KW_AS
427 yard_opt_115 -> 
428 yard_opt_116 -> yard_exp_brackets_224
429 yard_opt_116 -> 
430 yard_opt_118 -> KW_AS
431 yard_opt_118 -> 
432 yard_opt_117 -> yard_exp_brackets_225
433 yard_opt_117 -> 
434 yard_opt_119 -> yard_exp_brackets_226
435 yard_opt_119 -> 
436 yard_opt_120 -> yard_exp_brackets_227
437 yard_opt_120 -> 
438 rowset_function -> openrowset
439 rowset_function -> openquery
440 rowset_function -> opendatasource
441 rowset_function -> freetexttable
442 rowset_function -> containstable
443 openrowset -> KW_OPENROWSET LPAREN yard_exp_brackets_228 RPAREN
444 yard_opt_121 -> yard_exp_brackets_229
445 yard_opt_121 -> 
446 yard_opt_122 -> yard_exp_brackets_230
447 yard_opt_122 -> 
448 yard_opt_123 -> bulk_options
449 yard_opt_123 -> 
450 bulk_options -> yard_opt_124 yard_opt_125 yard_opt_126 yard_opt_127 yard_opt_128 yard_opt_129
451 yard_opt_124 -> yard_exp_brackets_231
452 yard_opt_124 -> 
453 yard_opt_125 -> yard_exp_brackets_232
454 yard_opt_125 -> 
455 yard_opt_126 -> yard_exp_brackets_233
456 yard_opt_126 -> 
457 yard_opt_127 -> yard_exp_brackets_234
458 yard_opt_127 -> 
459 yard_opt_128 -> yard_exp_brackets_235
460 yard_opt_128 -> 
461 yard_opt_129 -> yard_exp_brackets_236
462 yard_opt_129 -> 
463 openquery -> KW_OPENQUERY LPAREN full_ident COMMA STRING_CONST RPAREN
464 opendatasource -> KW_OPENDATASOURCE LPAREN ident COMMA STRING_CONST RPAREN
465 freetexttable -> KW_FREETEXTTABLE LPAREN full_ident COMMA yard_exp_brackets_237 COMMA STRING_CONST yard_opt_130 yard_opt_131 RPAREN
466 yard_rule_comma_list_50 -> yard_rule_list_51
467 yard_rule_list_51 -> full_ident yard_rule_yard_many_3_52
468 yard_rule_list_51 -> 
469 yard_rule_yard_many_3_52 -> yard_exp_brackets_238 yard_rule_yard_many_3_52
470 yard_rule_yard_many_3_52 -> 
471 yard_opt_130 -> yard_exp_brackets_239
472 yard_opt_130 -> 
473 yard_opt_131 -> yard_exp_brackets_240
474 yard_opt_131 -> 
475 containstable -> KW_CONTAINSTABLE LPAREN full_ident COMMA yard_exp_brackets_241 COMMA EMPTY contains_search_condition EMPTY yard_opt_132 yard_opt_133 RPAREN
476 yard_opt_132 -> yard_exp_brackets_242
477 yard_opt_132 -> 
478 yard_opt_133 -> yard_exp_brackets_243
479 yard_opt_133 -> 
480 contains_search_condition -> yard_exp_brackets_245
481 contains_search_condition -> yard_exp_brackets_244
482 yard_some_3 -> contains_search_condition yard_some_3
483 yard_some_3 -> contains_search_condition
484 simple_term -> STRING_CONST
485 prefix_term -> STRING_CONST
486 generation_term -> KW_FORMSOF LPAREN yard_exp_brackets_246 COMMA yard_rule_comma_list_53 RPAREN
487 yard_rule_comma_list_53 -> yard_rule_list_54
488 yard_rule_list_54 -> simple_term yard_rule_yard_many_3_55
489 yard_rule_list_54 -> 
490 yard_rule_yard_many_3_55 -> yard_exp_brackets_247 yard_rule_yard_many_3_55
491 yard_rule_yard_many_3_55 -> 
492 proximity_term -> yard_exp_brackets_248 yard_some_4
493 yard_some_4 -> yard_exp_brackets_250 yard_some_4
494 yard_some_4 -> yard_exp_brackets_249
495 weighted_term -> KW_ISABOUT LPAREN yard_rule_comma_list_56 RPAREN
496 yard_rule_comma_list_56 -> yard_rule_list_57
497 yard_rule_list_57 -> yard_exp_brackets_251 yard_rule_yard_many_3_58
498 yard_rule_list_57 -> 
499 yard_rule_yard_many_3_58 -> yard_exp_brackets_252 yard_rule_yard_many_3_58
500 yard_rule_yard_many_3_58 -> 
501 yard_opt_134 -> yard_exp_brackets_253
502 yard_opt_134 -> 
503 table_hint -> yard_opt_135 yard_exp_brackets_254
504 yard_opt_135 -> KW_NOEXPAND
505 yard_opt_135 -> 
506 tablesample_clause -> KW_TABLESAMPLE yard_opt_137 LPAREN sql_expr yard_opt_138 RPAREN yard_opt_139
507 yard_opt_137 -> KW_SYSTEM
508 yard_opt_137 -> 
509 yard_opt_138 -> yard_exp_brackets_256
510 yard_opt_138 -> 
511 yard_opt_139 -> yard_exp_brackets_257
512 yard_opt_139 -> 
513 joined_table -> LPAREN joined_table RPAREN
514 joined_table -> table_source KW_CROSS KW_JOIN table_source
515 joined_table -> table_source join_type table_source KW_ON search_condition
516 join_type -> yard_opt_140 KW_JOIN
517 yard_opt_141 -> KW_OUTER
518 yard_opt_141 -> 
519 yard_opt_142 -> join_hint
520 yard_opt_142 -> 
521 yard_opt_140 -> yard_exp_brackets_258
522 yard_opt_140 -> 
523 join_hint -> KW_REMOTE
524 join_hint -> KW_MERGE
525 join_hint -> KW_HASH
526 join_hint -> KW_LOOP
527 pivot_clause -> LPAREN yard_some_5 KW_FOR sql_expr KW_IN LPAREN yard_rule_comma_list_7 RPAREN RPAREN
528 yard_some_5 -> sql_expr yard_some_5
529 yard_some_5 -> sql_expr
530 unpivot_clause -> LPAREN ident KW_FOR sql_expr KW_IN LPAREN yard_rule_comma_list_50 RPAREN RPAREN
531 case -> KW_CASE yard_opt_143 yard_many_12 yard_opt_144 KW_END
532 yard_opt_143 -> sql_expr
533 yard_opt_143 -> 
534 yard_many_12 -> yard_exp_brackets_259 yard_many_12
535 yard_many_12 -> 
536 yard_opt_144 -> yard_exp_brackets_260
537 yard_opt_144 -> 
538 create_table -> KW_CREATE KW_TABLE yard_opt_145 IDENT LPAREN yard_rule_comma_list_59 yard_rule_comma_list_62 RPAREN yard_opt_148 yard_opt_149 yard_opt_150
539 yard_rule_comma_list_62 -> yard_rule_list_63
540 yard_rule_list_63 -> table_constraint_create_table yard_rule_yard_many_3_64
541 yard_rule_list_63 -> 
542 yard_rule_yard_many_3_64 -> yard_exp_brackets_261 yard_rule_yard_many_3_64
543 yard_rule_yard_many_3_64 -> 
544 yard_rule_comma_list_59 -> yard_rule_list_60
545 yard_rule_list_60 -> yard_exp_brackets_262 yard_rule_yard_many_3_61
546 yard_rule_list_60 -> 
547 yard_rule_yard_many_3_61 -> yard_exp_brackets_263 yard_rule_yard_many_3_61
548 yard_rule_yard_many_3_61 -> 
549 yard_opt_146 -> IDENT
550 yard_opt_146 -> 
551 yard_opt_147 -> DOT
552 yard_opt_147 -> 
553 yard_opt_145 -> yard_exp_brackets_264
554 yard_opt_145 -> 
555 yard_opt_148 -> yard_exp_brackets_265
556 yard_opt_148 -> 
557 yard_opt_149 -> yard_exp_brackets_266
558 yard_opt_149 -> 
559 yard_opt_150 -> SEMI
560 yard_opt_150 -> 
561 column_definition_create_table -> yard_opt_155 yard_opt_158 yard_many_13
562 column_definition_create_table -> ident sql_datatype yard_opt_151 yard_opt_152 yard_opt_153
563 yard_opt_151 -> yard_exp_brackets_267
564 yard_opt_151 -> 
565 yard_opt_152 -> yard_exp_brackets_268
566 yard_opt_152 -> 
567 yard_opt_154 -> yard_exp_brackets_269
568 yard_opt_154 -> 
569 yard_opt_153 -> yard_exp_brackets_270
570 yard_opt_153 -> 
571 yard_opt_156 -> yard_exp_brackets_271
572 yard_opt_156 -> 
573 yard_opt_157 -> yard_exp_brackets_272
574 yard_opt_157 -> 
575 yard_opt_155 -> yard_exp_brackets_273
576 yard_opt_155 -> 
577 yard_opt_158 -> KW_ROWGUIDCOL
578 yard_opt_158 -> 
579 yard_many_13 -> column_constraint_create_table yard_many_13
580 yard_many_13 -> 
581 column_constraint_create_table -> yard_opt_159 yard_exp_brackets_274
582 yard_opt_159 -> yard_exp_brackets_275
583 yard_opt_159 -> 
584 yard_opt_160 -> yard_exp_brackets_276
585 yard_opt_160 -> 
586 yard_opt_161 -> yard_exp_brackets_277
587 yard_opt_161 -> 
588 yard_rule_comma_list_65 -> yard_rule_list_66
589 yard_rule_list_66 -> index_option yard_rule_yard_many_3_67
590 yard_rule_list_66 -> 
591 yard_rule_yard_many_3_67 -> yard_exp_brackets_278 yard_rule_yard_many_3_67
592 yard_rule_yard_many_3_67 -> 
593 yard_opt_162 -> yard_exp_brackets_279
594 yard_opt_162 -> 
595 yard_opt_163 -> yard_exp_brackets_280
596 yard_opt_163 -> 
597 yard_opt_164 -> yard_exp_brackets_281
598 yard_opt_164 -> 
599 yard_opt_165 -> yard_exp_brackets_282
600 yard_opt_165 -> 
601 yard_many_14 -> delete_update yard_many_14
602 yard_many_14 -> 
603 yard_opt_166 -> yard_exp_brackets_283
604 yard_opt_166 -> 
605 yard_opt_167 -> yard_exp_brackets_284
606 yard_opt_167 -> 
607 computed_column_definition -> IDENT KW_AS sql_expr yard_opt_168 yard_opt_170
608 yard_opt_169 -> yard_exp_brackets_285
609 yard_opt_169 -> 
610 yard_opt_168 -> yard_exp_brackets_286
611 yard_opt_168 -> 
612 yard_opt_171 -> yard_exp_brackets_287
613 yard_opt_171 -> 
614 yard_opt_172 -> yard_exp_brackets_288
615 yard_opt_172 -> 
616 yard_opt_173 -> yard_exp_brackets_289
617 yard_opt_173 -> 
618 yard_opt_174 -> yard_exp_brackets_290
619 yard_opt_174 -> 
620 yard_opt_175 -> yard_exp_brackets_291
621 yard_opt_175 -> 
622 yard_opt_176 -> yard_exp_brackets_292
623 yard_opt_176 -> 
624 yard_many_15 -> delete_update yard_many_15
625 yard_many_15 -> 
626 yard_opt_177 -> yard_exp_brackets_293
627 yard_opt_177 -> 
628 yard_opt_178 -> yard_exp_brackets_294
629 yard_opt_178 -> 
630 yard_opt_170 -> yard_exp_brackets_295
631 yard_opt_170 -> 
632 table_constraint_create_table -> yard_opt_179 yard_exp_brackets_296
633 yard_rule_comma_list_68 -> yard_rule_list_69
634 yard_rule_list_69 -> yard_exp_brackets_297 yard_rule_yard_many_3_70
635 yard_rule_list_69 -> 
636 yard_rule_yard_many_3_70 -> yard_exp_brackets_298 yard_rule_yard_many_3_70
637 yard_rule_yard_many_3_70 -> 
638 yard_rule_comma_list_71 -> yard_rule_list_72
639 yard_rule_list_72 -> IDENT yard_rule_yard_many_3_73
640 yard_rule_list_72 -> 
641 yard_rule_yard_many_3_73 -> yard_exp_brackets_299 yard_rule_yard_many_3_73
642 yard_rule_yard_many_3_73 -> 
643 yard_opt_179 -> yard_exp_brackets_300
644 yard_opt_179 -> 
645 yard_opt_180 -> yard_exp_brackets_301
646 yard_opt_180 -> 
647 yard_opt_181 -> yard_exp_brackets_302
648 yard_opt_181 -> 
649 yard_opt_182 -> yard_exp_brackets_303
650 yard_opt_182 -> 
651 yard_opt_183 -> yard_exp_brackets_304
652 yard_opt_183 -> 
653 yard_opt_184 -> yard_exp_brackets_305
654 yard_opt_184 -> 
655 yard_many_16 -> delete_update yard_many_16
656 yard_many_16 -> 
657 yard_opt_185 -> yard_exp_brackets_306
658 yard_opt_185 -> 
659 yard_opt_186 -> yard_exp_brackets_307
660 yard_opt_186 -> 
661 delete_update -> KW_ON yard_exp_brackets_308 delete_update_options
662 delete_update_options -> KW_SET KW_DEFAULT
663 delete_update_options -> KW_SET KW_NULL
664 delete_update_options -> KW_CASCADE
665 delete_update_options -> KW_NO KW_ACTION
666 index_option -> yard_exp_brackets_309
667 insert -> yard_opt_187 KW_INSERT yard_opt_188 yard_opt_190 yard_exp_brackets_310 yard_exp_brackets_311 yard_opt_195
668 yard_opt_187 -> yard_exp_brackets_312
669 yard_opt_187 -> 
670 yard_opt_189 -> KW_PERCENT
671 yard_opt_189 -> 
672 yard_opt_188 -> yard_exp_brackets_313
673 yard_opt_188 -> 
674 yard_opt_190 -> KW_INTO
675 yard_opt_190 -> 
676 yard_many_17 -> table_hint yard_many_17
677 yard_many_17 -> 
678 yard_opt_191 -> yard_exp_brackets_314
679 yard_opt_191 -> 
680 yard_opt_192 -> yard_exp_brackets_315
681 yard_opt_192 -> 
682 yard_rule_comma_list_74 -> yard_rule_list_75
683 yard_rule_list_75 -> LOCALVAR yard_rule_yard_many_3_76
684 yard_rule_list_75 -> 
685 yard_rule_yard_many_3_76 -> yard_exp_brackets_316 yard_rule_yard_many_3_76
686 yard_rule_yard_many_3_76 -> 
687 yard_opt_193 -> output_clause
688 yard_opt_193 -> 
689 yard_opt_194 -> yard_exp_brackets_317
690 yard_opt_194 -> 
691 yard_rule_comma_list_77 -> yard_rule_list_78
692 yard_rule_list_78 -> yard_exp_brackets_318 yard_rule_yard_many_3_79
693 yard_rule_list_78 -> 
694 yard_rule_yard_many_3_79 -> yard_exp_brackets_319 yard_rule_yard_many_3_79
695 yard_rule_yard_many_3_79 -> 
696 yard_opt_195 -> SEMI
697 yard_opt_195 -> 
698 output_clause -> KW_OUTPUT dml_select_list KW_INTO yard_exp_brackets_320 yard_opt_196 yard_opt_197
699 yard_opt_196 -> yard_exp_brackets_321
700 yard_opt_196 -> 
701 yard_opt_197 -> yard_exp_brackets_322
702 yard_opt_197 -> 
703 dml_select_list -> yard_exp_brackets_323 yard_opt_198 yard_rule_comma_list_80
704 yard_rule_comma_list_80 -> yard_rule_list_81
705 yard_rule_list_81 -> yard_exp_brackets_324 yard_rule_yard_many_3_82
706 yard_rule_list_81 -> 
707 yard_rule_yard_many_3_82 -> yard_exp_brackets_325 yard_rule_yard_many_3_82
708 yard_rule_yard_many_3_82 -> 
709 yard_opt_199 -> KW_AS
710 yard_opt_199 -> 
711 yard_opt_198 -> yard_exp_brackets_326
712 yard_opt_198 -> 
713 yard_opt_201 -> KW_AS
714 yard_opt_201 -> 
715 yard_opt_200 -> yard_exp_brackets_327
716 yard_opt_200 -> 
717 object -> yard_opt_202 ident
718 yard_opt_203 -> IDENT
719 yard_opt_203 -> 
720 yard_opt_202 -> yard_exp_brackets_330
721 yard_opt_202 -> 
722 begin_transaction -> KW_BEGIN yard_exp_brackets_331 yard_opt_204 yard_opt_207
723 yard_opt_206 -> STRING_CONST
724 yard_opt_206 -> 
725 yard_opt_205 -> yard_exp_brackets_332
726 yard_opt_205 -> 
727 yard_opt_204 -> yard_exp_brackets_333
728 yard_opt_204 -> 
729 yard_opt_207 -> SEMI
730 yard_opt_207 -> 
731 commit_transaction -> KW_COMMIT yard_exp_brackets_334 yard_opt_208 yard_opt_209
732 yard_opt_208 -> yard_exp_brackets_335
733 yard_opt_208 -> 
734 yard_opt_209 -> SEMI
735 yard_opt_209 -> 
736 execute_stmnt -> execute_command
737 execute_stmnt -> execute_character_string
738 execute_stmnt -> execute_proc
739 execute_proc -> yard_opt_210 yard_exp_brackets_336
740 yard_rule_brace_opt_86 -> LPAREN yard_rule_comma_list_83 RPAREN
741 yard_rule_brace_opt_86 -> yard_rule_comma_list_83
742 yard_rule_comma_list_83 -> yard_rule_list_84
743 yard_rule_list_84 -> yard_opt_213 yard_rule_yard_many_3_85
744 yard_rule_list_84 -> 
745 yard_rule_yard_many_3_85 -> yard_exp_brackets_337 yard_rule_yard_many_3_85
746 yard_rule_yard_many_3_85 -> 
747 yard_rule_comma_list_87 -> yard_rule_list_88
748 yard_rule_list_88 -> yard_exp_brackets_338 yard_rule_yard_many_3_89
749 yard_rule_list_88 -> 
750 yard_rule_yard_many_3_89 -> yard_exp_brackets_339 yard_rule_yard_many_3_89
751 yard_rule_yard_many_3_89 -> 
752 yard_opt_210 -> yard_exp_brackets_340
753 yard_opt_210 -> 
754 yard_opt_211 -> yard_exp_brackets_341
755 yard_opt_211 -> 
756 yard_opt_212 -> yard_exp_brackets_342
757 yard_opt_212 -> 
758 yard_opt_214 -> yard_exp_brackets_343
759 yard_opt_214 -> 
760 yard_opt_215 -> yard_exp_brackets_344
761 yard_opt_215 -> 
762 yard_opt_216 -> KW_DEFAULT
763 yard_opt_216 -> 
764 yard_opt_213 -> yard_exp_brackets_345
765 yard_opt_213 -> 
766 yard_opt_217 -> yard_exp_brackets_346
767 yard_opt_217 -> 
768 yard_opt_218 -> yard_exp_brackets_347
769 yard_opt_218 -> 
770 create_object -> STOREDPROCEDURE LPAREN yard_rule_comma_list_11 RPAREN
771 execute_character_string -> yard_exp_brackets_348 LPAREN yard_some_6 RPAREN yard_opt_220 yard_opt_221
772 yard_opt_219 -> PLUS
773 yard_opt_219 -> 
774 yard_some_6 -> yard_exp_brackets_350 yard_some_6
775 yard_some_6 -> yard_exp_brackets_349
776 yard_opt_220 -> yard_exp_brackets_351
777 yard_opt_220 -> 
778 yard_opt_221 -> SEMI
779 yard_opt_221 -> 
780 execute_command -> yard_exp_brackets_352 LPAREN yard_some_7 yard_opt_224 RPAREN yard_opt_226 yard_opt_227 yard_opt_228
781 yard_opt_222 -> EMPTY
782 yard_opt_222 -> 
783 yard_opt_223 -> PLUS
784 yard_opt_223 -> 
785 yard_some_7 -> yard_exp_brackets_354 yard_some_7
786 yard_some_7 -> yard_exp_brackets_353
787 yard_opt_225 -> KW_OUTPUT
788 yard_opt_225 -> 
789 yard_some_8 -> yard_exp_brackets_356 yard_some_8
790 yard_some_8 -> yard_exp_brackets_355
791 yard_opt_224 -> yard_some_8
792 yard_opt_224 -> 
793 yard_opt_226 -> yard_exp_brackets_357
794 yard_opt_226 -> 
795 yard_opt_227 -> yard_exp_brackets_358
796 yard_opt_227 -> 
797 yard_opt_228 -> SEMI
798 yard_opt_228 -> 
799 type_size -> LPAREN yard_exp_brackets_359 RPAREN
800 sql_datatype -> KW_SQL_VARIANT
801 sql_datatype -> KW_NVARCHAR yard_opt_234
802 sql_datatype -> KW_NCHAR yard_opt_233
803 sql_datatype -> KW_VARBINARY yard_opt_232
804 sql_datatype -> KW_VARCHAR yard_opt_231
805 sql_datatype -> KW_CHAR yard_opt_230
806 sql_datatype -> KW_REAL
807 sql_datatype -> KW_FLOAT yard_opt_229
808 sql_datatype -> KW_MONEY
809 sql_datatype -> KW_TINYINT
810 sql_datatype -> KW_INT
811 sql_datatype -> KW_SYSNAME
812 sql_datatype -> KW_SMALLMONEY
813 sql_datatype -> KW_DATETIME
814 sql_datatype -> KW_DECIMAL
815 sql_datatype -> KW_SMALLINT
816 sql_datatype -> KW_BIT
817 sql_datatype -> KW_NUMERIC
818 sql_datatype -> KW_BIGINT
819 yard_opt_229 -> type_size
820 yard_opt_229 -> 
821 yard_opt_230 -> type_size
822 yard_opt_230 -> 
823 yard_opt_231 -> type_size
824 yard_opt_231 -> 
825 yard_opt_232 -> type_size
826 yard_opt_232 -> 
827 yard_opt_233 -> type_size
828 yard_opt_233 -> 
829 yard_opt_234 -> type_size
830 yard_opt_234 -> 
831 execute_as -> yard_exp_brackets_360 KW_AS yard_exp_brackets_361
832 binop -> yard_exp_brackets_365
833 binop -> yard_exp_brackets_364
834 binop -> yard_exp_brackets_363
835 binop -> yard_exp_brackets_362
836 binop -> OP_MT
837 binop -> OP_LT
838 binop -> OP_TILDA
839 binop -> OP_MOD
840 binop -> OP_DIV
841 binop -> STAR
842 binop -> OP_MINUS
843 binop -> OP_PLUS
844 binop -> OP_EQ
845 binop -> OP_OR_EQ
846 binop -> OP_XOR_EQ
847 binop -> OP_AND_EQ
848 binop -> OP_MOD_EQ
849 binop -> OP_DIV_EQ
850 binop -> OP_MUL_EQ
851 binop -> OP_MINUS_EQ
852 binop -> OP_PLUS_EQ
853 fetch_stmnt -> KW_FETCH yard_opt_235 yard_exp_brackets_366 yard_opt_238
854 yard_opt_236 -> yard_exp_brackets_367
855 yard_opt_236 -> 
856 yard_opt_235 -> yard_exp_brackets_368
857 yard_opt_235 -> 
858 yard_opt_237 -> KW_GLOBAL
859 yard_opt_237 -> 
860 yard_opt_238 -> yard_exp_brackets_369
861 yard_opt_238 -> 
862 sql_expr -> fetch_stmnt yard_lr_sql_expr
863 sql_expr -> KW_TYPE KW_FROM full_ident yard_lr_sql_expr
864 sql_expr -> KW_IS_MEMBER LPAREN STRING_CONST RPAREN yard_lr_sql_expr
865 sql_expr -> KW_EXISTS sql_expr yard_lr_sql_expr
866 sql_expr -> begin_transaction yard_lr_sql_expr
867 sql_expr -> case yard_lr_sql_expr
868 sql_expr -> aggregate_windowed_function yard_lr_sql_expr
869 sql_expr -> ranking_windowed_function yard_lr_sql_expr
870 sql_expr -> STAR yard_lr_sql_expr
871 sql_expr -> LOCALVAR yard_lr_sql_expr
872 sql_expr -> GLOBALVAR yard_lr_sql_expr
873 sql_expr -> yard_exp_brackets_370 sql_expr yard_lr_sql_expr
874 sql_expr -> select_stmnt yard_lr_sql_expr
875 sql_expr -> full_ident yard_lr_sql_expr
876 sql_expr -> call_expr yard_lr_sql_expr
877 sql_expr -> LPAREN sql_expr RPAREN yard_lr_sql_expr
878 sql_expr -> scalar_function yard_lr_sql_expr
879 sql_expr -> sql_value yard_lr_sql_expr
880 yard_lr_sql_expr -> yard_opt_241 KW_LIKE sql_expr yard_lr_sql_expr
881 yard_lr_sql_expr -> yard_opt_240 KW_IN LPAREN yard_rule_comma_list_11 RPAREN yard_lr_sql_expr
882 yard_lr_sql_expr -> yard_exp_brackets_371 sql_expr yard_lr_sql_expr
883 yard_lr_sql_expr -> KW_COLLATE ident yard_lr_sql_expr
884 yard_lr_sql_expr -> KW_IS yard_opt_239 KW_NULL yard_lr_sql_expr
885 yard_lr_sql_expr -> binop sql_expr yard_lr_sql_expr
886 yard_lr_sql_expr -> 
887 yard_opt_239 -> KW_NOT
888 yard_opt_239 -> 
889 yard_opt_240 -> KW_NOT
890 yard_opt_240 -> 
891 yard_opt_241 -> KW_NOT
892 yard_opt_241 -> 
893 call_expr -> full_ident LPAREN yard_rule_comma_list_90 RPAREN
894 yard_rule_comma_list_90 -> yard_rule_list_91
895 yard_rule_list_91 -> yard_exp_brackets_372 yard_rule_yard_many_3_92
896 yard_rule_list_91 -> 
897 yard_rule_yard_many_3_92 -> yard_exp_brackets_373 yard_rule_yard_many_3_92
898 yard_rule_yard_many_3_92 -> 
899 yard_exp_brackets_96 -> batch_body yard_opt_2
900 yard_exp_brackets_97 -> KW_TRANSACTION
901 yard_exp_brackets_97 -> KW_TRAN
902 yard_exp_brackets_98 -> LOCALVAR
903 yard_exp_brackets_98 -> IDENT
904 yard_exp_brackets_99 -> KW_XACT_ABORT
905 yard_exp_brackets_99 -> KW_STATISTICS yard_exp_brackets_374
906 yard_exp_brackets_99 -> KW_SHOWPLAN_XML
907 yard_exp_brackets_99 -> KW_SHOWPLAN_TEXT
908 yard_exp_brackets_99 -> KW_SHOWPLAN_ALL
909 yard_exp_brackets_99 -> KW_REMOTE_PROC_TRANSACTIONS
910 yard_exp_brackets_99 -> KW_PARSEONLY
911 yard_exp_brackets_99 -> KW_NUMERIC_ROUNDABORT
912 yard_exp_brackets_99 -> KW_NOEXEC
913 yard_exp_brackets_99 -> KW_NOCOUNT
914 yard_exp_brackets_99 -> KW_IMPLICIT_TRANSACTIONS
915 yard_exp_brackets_99 -> KW_FORCEPLAN
916 yard_exp_brackets_99 -> KW_FMTONLY
917 yard_exp_brackets_99 -> KW_QUOTED_IDENTIFIER
918 yard_exp_brackets_99 -> KW_CURSOR_CLOSE_ON_COMMIT
919 yard_exp_brackets_99 -> KW_CONCAT_NULL_YIELDS_NULL
920 yard_exp_brackets_99 -> KW_ANSI_WARNINGS
921 yard_exp_brackets_99 -> KW_ANSI_PADDING
922 yard_exp_brackets_99 -> KW_ANSI_NULL_DFLT
923 yard_exp_brackets_99 -> KW_ANSI_DEFAULTS
924 yard_exp_brackets_99 -> KW_ANSI_NULLS
925 yard_exp_brackets_100 -> KW_OFF
926 yard_exp_brackets_100 -> KW_ON
927 yard_exp_brackets_101 -> STRING_CONST yard_exp_brackets_375 ident LPAREN yard_rule_comma_list_4 RPAREN
928 yard_exp_brackets_101 -> LOCALVAR yard_opt_6 OP_EQ sql_expr
929 yard_exp_brackets_102 -> yard_exp_brackets_376
930 yard_exp_brackets_102 -> ident
931 yard_exp_brackets_102 -> LOCALVAR OP_EQ LOCALVAR
932 yard_exp_brackets_103 -> COMMA proc_formal_param
933 yard_exp_brackets_104 -> DOUBLE_COLON ident
934 yard_exp_brackets_105 -> KW_SCROLL
935 yard_exp_brackets_105 -> KW_FORWARD_ONLY
936 yard_exp_brackets_106 -> KW_FAST_FORWARD
937 yard_exp_brackets_106 -> KW_DYNAMIC
938 yard_exp_brackets_106 -> KW_KEYSET
939 yard_exp_brackets_106 -> KW_STATIC
940 yard_exp_brackets_107 -> KW_OPTIMISTIC
941 yard_exp_brackets_107 -> KW_SCROLL_LOCKS
942 yard_exp_brackets_107 -> KW_READ_ONLY
943 yard_exp_brackets_108 -> KW_OF yard_rule_comma_list_7
944 yard_exp_brackets_109 -> COMMA ident
945 yard_exp_brackets_110 -> KW_FOR yard_exp_brackets_377
946 yard_exp_brackets_111 -> KW_PROCEDURE
947 yard_exp_brackets_111 -> KW_PROC
948 yard_exp_brackets_112 -> ident DOT
949 yard_exp_brackets_113 -> yard_opt_13 ident
950 yard_exp_brackets_114 -> yard_opt_13 ident
951 yard_exp_brackets_115 -> full_ident DOT
952 yard_exp_brackets_116 -> OP_EQ yard_exp_brackets_378
953 yard_exp_brackets_117 -> KW_OUTPUT
954 yard_exp_brackets_117 -> KW_OUT
955 yard_exp_brackets_118 -> KW_PROC
956 yard_exp_brackets_118 -> KW_PROCEDURE
957 yard_exp_brackets_119 -> ident DOT
958 yard_exp_brackets_120 -> SEMI DEC_NUMBER
959 yard_exp_brackets_121 -> KW_WITH yard_some_2
960 yard_exp_brackets_122 -> KW_FOR KW_REPLICATION
961 yard_exp_brackets_123 -> proc_body_stmnt yard_opt_24
962 yard_exp_brackets_124 -> proc_body_stmnt yard_opt_30
963 yard_exp_brackets_125 -> KW_ELSE proc_body_stmnt
964 yard_exp_brackets_126 -> LOCALVAR
965 yard_exp_brackets_126 -> STRING_CONST
966 yard_exp_brackets_126 -> dec_num
967 yard_exp_brackets_127 -> COMMA dec_num COMMA dec_num
968 yard_exp_brackets_128 -> COMMA yard_rule_comma_list_11
969 yard_exp_brackets_129 -> COMMA sql_expr
970 yard_exp_brackets_130 -> KW_WITH yard_rule_comma_list_14
971 yard_exp_brackets_131 -> KW_SETERROR
972 yard_exp_brackets_131 -> KW_NOWAIT
973 yard_exp_brackets_131 -> KW_LOG
974 yard_exp_brackets_132 -> COMMA yard_exp_brackets_379
975 yard_exp_brackets_133 -> ident yard_opt_38 table_type_definition
976 yard_exp_brackets_133 -> ident KW_CURSOR yard_opt_35 yard_opt_36 yard_opt_37 sql_expr
977 yard_exp_brackets_133 -> LOCALVAR yard_opt_34 sql_datatype
978 yard_exp_brackets_134 -> COMMA yard_exp_brackets_380
979 yard_exp_brackets_135 -> table_constraint
980 yard_exp_brackets_135 -> column_definition
981 yard_exp_brackets_136 -> yard_exp_brackets_381
982 yard_exp_brackets_136 -> KW_SCALAR_DATA_TYPE
983 yard_exp_brackets_137 -> KW_COLLATE ident
984 yard_exp_brackets_138 -> KW_DEFAULT sql_expr
985 yard_exp_brackets_139 -> DEC_NUMBER COMMA DEC_NUMBER
986 yard_exp_brackets_140 -> KW_IDENTITY yard_opt_43
987 yard_exp_brackets_140 -> yard_opt_42
988 yard_exp_brackets_141 -> KW_CHECK sql_expr
989 yard_exp_brackets_141 -> yard_opt_47
990 yard_exp_brackets_141 -> yard_opt_46
991 yard_exp_brackets_142 -> KW_NOT KW_NULL
992 yard_exp_brackets_142 -> KW_NULL
993 yard_exp_brackets_143 -> KW_UNIQUE
994 yard_exp_brackets_143 -> KW_PRIMARY KW_KEY
995 yard_exp_brackets_144 -> yard_exp_brackets_382 LPAREN yard_rule_comma_list_7 RPAREN
996 yard_exp_brackets_145 -> KW_CHECK LPAREN search_condition RPAREN
997 yard_exp_brackets_146 -> yard_opt_48 query_expression yard_opt_49 yard_opt_51 yard_opt_53 yard_opt_54
998 yard_exp_brackets_147 -> KW_WITH yard_rule_comma_list_20
999 yard_exp_brackets_148 -> COMMA common_table_expression
1000 yard_exp_brackets_149 -> KW_DESC
1001 yard_exp_brackets_149 -> KW_ASC
1002 yard_exp_brackets_150 -> KW_ORDER KW_BY yard_rule_comma_list_23
1003 yard_exp_brackets_151 -> yard_exp_brackets_383 yard_opt_50
1004 yard_exp_brackets_152 -> COMMA yard_exp_brackets_384
1005 yard_exp_brackets_153 -> KW_BY yard_rule_comma_list_11
1006 yard_exp_brackets_154 -> KW_COMPUTE yard_rule_comma_list_26 yard_opt_52
1007 yard_exp_brackets_155 -> yard_exp_brackets_385 sql_expr
1008 yard_exp_brackets_156 -> COMMA yard_exp_brackets_386
1009 yard_exp_brackets_157 -> KW_OPTION LPAREN yard_rule_comma_list_29 RPAREN
1010 yard_exp_brackets_158 -> COMMA query_hint
1011 yard_exp_brackets_159 -> KW_FOR yard_exp_brackets_387
1012 yard_exp_brackets_160 -> KW_PATH yard_opt_65 yard_opt_66
1013 yard_exp_brackets_160 -> KW_EXPLICIT yard_opt_63
1014 yard_exp_brackets_160 -> yard_exp_brackets_388 yard_opt_58
1015 yard_exp_brackets_161 -> LPAREN STRING_CONST RPAREN
1016 yard_exp_brackets_162 -> LPAREN STRING_CONST RPAREN
1017 yard_exp_brackets_163 -> COMMA yard_exp_brackets_389
1018 yard_exp_brackets_164 -> KW_ABSENT
1019 yard_exp_brackets_164 -> KW_XSINIL
1020 yard_exp_brackets_165 -> COMMA KW_ELEMENTS yard_opt_62
1021 yard_exp_brackets_166 -> common_directives yard_opt_59 yard_opt_61
1022 yard_exp_brackets_167 -> COMMA KW_XMLDATA
1023 yard_exp_brackets_168 -> common_directives yard_opt_64
1024 yard_exp_brackets_169 -> LPAREN STRING_CONST RPAREN
1025 yard_exp_brackets_170 -> KW_ABSENT
1026 yard_exp_brackets_170 -> KW_XSINIL
1027 yard_exp_brackets_171 -> COMMA KW_ELEMENTS yard_opt_68
1028 yard_exp_brackets_172 -> common_directives yard_opt_67
1029 yard_exp_brackets_173 -> COMMA KW_BINARY KW_BASE64
1030 yard_exp_brackets_174 -> COMMA KW_TYPE
1031 yard_exp_brackets_175 -> LPAREN STRING_CONST RPAREN
1032 yard_exp_brackets_176 -> COMMA KW_ROOT yard_opt_72
1033 yard_exp_brackets_177 -> KW_USE KW_PLAN STRING_CONST
1034 yard_exp_brackets_177 -> KW_MAXRECURSION DEC_NUMBER
1035 yard_exp_brackets_177 -> KW_EXPAND KW_VIEWS
1036 yard_exp_brackets_177 -> KW_KEEPFIXED KW_PLAN
1037 yard_exp_brackets_177 -> KW_KEEP KW_PLAN
1038 yard_exp_brackets_177 -> KW_ROBUST KW_PLAN
1039 yard_exp_brackets_177 -> KW_RECOMPILE
1040 yard_exp_brackets_177 -> KW_PARAMETERIZATION yard_exp_brackets_393
1041 yard_exp_brackets_177 -> KW_OPTIMIZE KW_FOR LPAREN yard_rule_comma_list_32 RPAREN
1042 yard_exp_brackets_177 -> KW_MAXDOP DEC_NUMBER
1043 yard_exp_brackets_177 -> KW_FORCE KW_ORDER
1044 yard_exp_brackets_177 -> KW_FAST DEC_NUMBER
1045 yard_exp_brackets_177 -> yard_exp_brackets_392 KW_JOIN
1046 yard_exp_brackets_177 -> yard_exp_brackets_391 KW_UNION
1047 yard_exp_brackets_177 -> yard_exp_brackets_390 KW_GROUP
1048 yard_exp_brackets_178 -> LOCALVAR OP_EQ STRING_CONST
1049 yard_exp_brackets_179 -> COMMA yard_exp_brackets_394
1050 yard_exp_brackets_180 -> ident
1051 yard_exp_brackets_180 -> full_ident
1052 yard_exp_brackets_181 -> LPAREN yard_rule_comma_list_7 RPAREN
1053 yard_exp_brackets_182 -> LPAREN query_expression RPAREN
1054 yard_exp_brackets_182 -> query_specification
1055 yard_exp_brackets_183 -> yard_rule_comma_list_35
1056 yard_exp_brackets_183 -> yard_exp_brackets_395 query_specification
1057 yard_exp_brackets_184 -> LPAREN query_expression RPAREN
1058 yard_exp_brackets_185 -> COMMA yard_exp_brackets_396
1059 yard_exp_brackets_186 -> KW_DISTINCT
1060 yard_exp_brackets_186 -> KW_ALL
1061 yard_exp_brackets_187 -> KW_WITH KW_TIES
1062 yard_exp_brackets_188 -> KW_TOP sql_expr yard_opt_79 yard_opt_80
1063 yard_exp_brackets_189 -> KW_INTO full_ident
1064 yard_exp_brackets_190 -> KW_WHERE search_condition
1065 yard_exp_brackets_191 -> KW_WITH yard_exp_brackets_397
1066 yard_exp_brackets_192 -> KW_GROUP KW_BY yard_opt_85 yard_rule_comma_list_11 yard_opt_86
1067 yard_exp_brackets_193 -> KW_HAVING search_condition
1068 yard_exp_brackets_194 -> ident OP_EQ sql_expr
1069 yard_exp_brackets_194 -> yard_exp_brackets_398
1070 yard_exp_brackets_194 -> full_ident DOT STAR
1071 yard_exp_brackets_194 -> STAR
1072 yard_exp_brackets_195 -> COMMA yard_exp_brackets_399
1073 yard_exp_brackets_196 -> ident DOT
1074 yard_exp_brackets_197 -> yard_exp_brackets_400 yard_exp_brackets_401
1075 yard_exp_brackets_198 -> yard_opt_91 ident
1076 yard_exp_brackets_199 -> KW_DISTINCT
1077 yard_exp_brackets_199 -> KW_ALL
1078 yard_exp_brackets_200 -> COMMA yard_opt_93
1079 yard_exp_brackets_201 -> select_stmnt
1080 yard_exp_brackets_201 -> KW_PARTITION KW_BY LOCALVAR
1081 yard_exp_brackets_202 -> KW_NTILE
1082 yard_exp_brackets_202 -> KW_ROW_NUMBER
1083 yard_exp_brackets_202 -> KW_DENSE_RANK
1084 yard_exp_brackets_202 -> KW_RANK
1085 yard_exp_brackets_203 -> full_ident
1086 yard_exp_brackets_203 -> select_stmnt
1087 yard_exp_brackets_203 -> KW_PARTITION KW_BY LOCALVAR
1088 yard_exp_brackets_204 -> KW_DESC
1089 yard_exp_brackets_204 -> KW_ASC
1090 yard_exp_brackets_205 -> KW_ORDER KW_BY yard_rule_comma_list_44
1091 yard_exp_brackets_206 -> sql_expr yard_opt_97
1092 yard_exp_brackets_207 -> COMMA yard_exp_brackets_402
1093 yard_exp_brackets_208 -> LPAREN search_condition RPAREN
1094 yard_exp_brackets_208 -> yard_opt_98 predicate
1095 yard_exp_brackets_209 -> yard_exp_brackets_403 yard_opt_99 yard_exp_brackets_404
1096 yard_exp_brackets_210 -> KW_ANY
1097 yard_exp_brackets_210 -> KW_SOME
1098 yard_exp_brackets_210 -> KW_ALL
1099 yard_exp_brackets_211 -> yard_exp_brackets_405 COMMA yard_exp_brackets_406
1100 yard_exp_brackets_212 -> KW_ESCAPE STRING_CONST
1101 yard_exp_brackets_213 -> DOT IDENT
1102 yard_exp_brackets_214 -> LPAREN RPAREN
1103 yard_exp_brackets_215 -> KW_FROM yard_rule_comma_list_47
1104 yard_exp_brackets_216 -> COMMA table_source
1105 yard_exp_brackets_217 -> sql_expr yard_opt_112 yard_opt_114
1106 yard_exp_brackets_217 -> yard_exp_brackets_407
1107 yard_exp_brackets_219 -> KW_WITH LPAREN table_hint yard_many_11 RPAREN
1108 yard_exp_brackets_220 -> yard_opt_110 ident
1109 yard_exp_brackets_221 -> LPAREN yard_rule_comma_list_7 RPAREN
1110 yard_exp_brackets_222 -> yard_opt_113 ident
1111 yard_exp_brackets_223 -> LPAREN yard_rule_comma_list_7 RPAREN
1112 yard_exp_brackets_224 -> LPAREN yard_rule_comma_list_7 RPAREN
1113 yard_exp_brackets_225 -> yard_opt_118 full_ident
1114 yard_exp_brackets_226 -> KW_AS ident
1115 yard_exp_brackets_227 -> LPAREN yard_rule_comma_list_7 RPAREN
1116 yard_exp_brackets_228 -> KW_BULK STRING_CONST COMMA yard_exp_brackets_410
1117 yard_exp_brackets_228 -> STRING_CONST COMMA yard_exp_brackets_408 COMMA yard_exp_brackets_409
1118 yard_exp_brackets_229 -> full_ident DOT
1119 yard_exp_brackets_230 -> full_ident DOT
1120 yard_exp_brackets_231 -> COMMA KW_CODEPAGE OP_EQ STRING_CONST
1121 yard_exp_brackets_232 -> COMMA KW_ERRORFILE OP_EQ STRING_CONST
1122 yard_exp_brackets_233 -> COMMA KW_FIRSTROW OP_EQ DEC_NUMBER
1123 yard_exp_brackets_234 -> COMMA KW_LASTROW OP_EQ DEC_NUMBER
1124 yard_exp_brackets_235 -> COMMA KW_MAXERRORS OP_EQ DEC_NUMBER
1125 yard_exp_brackets_236 -> COMMA KW_ROWS_PER_BATCH OP_EQ DEC_NUMBER
1126 yard_exp_brackets_237 -> STAR
1127 yard_exp_brackets_237 -> LPAREN yard_rule_comma_list_50 RPAREN
1128 yard_exp_brackets_237 -> full_ident
1129 yard_exp_brackets_238 -> COMMA full_ident
1130 yard_exp_brackets_239 -> COMMA KW_LANGUAGE yard_exp_brackets_411
1131 yard_exp_brackets_240 -> COMMA DEC_NUMBER
1132 yard_exp_brackets_241 -> STAR
1133 yard_exp_brackets_241 -> LPAREN yard_rule_comma_list_7 RPAREN
1134 yard_exp_brackets_241 -> ident
1135 yard_exp_brackets_242 -> COMMA KW_LANGUAGE yard_exp_brackets_412
1136 yard_exp_brackets_243 -> COMMA DEC_NUMBER
1137 yard_exp_brackets_244 -> weighted_term
1138 yard_exp_brackets_244 -> proximity_term
1139 yard_exp_brackets_244 -> generation_term
1140 yard_exp_brackets_244 -> prefix_term
1141 yard_exp_brackets_244 -> simple_term
1142 yard_exp_brackets_245 -> LPAREN contains_search_condition RPAREN yard_exp_brackets_413 yard_some_3
1143 yard_exp_brackets_246 -> KW_THESAURUS
1144 yard_exp_brackets_246 -> KW_INFLECTIONAL
1145 yard_exp_brackets_247 -> COMMA simple_term
1146 yard_exp_brackets_248 -> prefix_term
1147 yard_exp_brackets_248 -> simple_term
1148 yard_exp_brackets_249 -> yard_exp_brackets_414 yard_exp_brackets_415
1149 yard_exp_brackets_250 -> yard_exp_brackets_416 yard_exp_brackets_417
1150 yard_exp_brackets_251 -> yard_exp_brackets_418 yard_opt_134
1151 yard_exp_brackets_252 -> COMMA yard_exp_brackets_419
1152 yard_exp_brackets_253 -> WEIGHT LPAREN DEC_NUMBER RPAREN
1153 yard_exp_brackets_254 -> KW_XLOCK
1154 yard_exp_brackets_254 -> KW_UPDLOCK
1155 yard_exp_brackets_254 -> KW_TABLOCKX
1156 yard_exp_brackets_254 -> KW_TABLOCK
1157 yard_exp_brackets_254 -> KW_SERIALIZABLE
1158 yard_exp_brackets_254 -> KW_ROWLOCK
1159 yard_exp_brackets_254 -> KW_REPEATABLEREAD
1160 yard_exp_brackets_254 -> KW_READUNCOMMITTED
1161 yard_exp_brackets_254 -> KW_READPAST
1162 yard_exp_brackets_254 -> KW_READCOMMITTEDLOCK
1163 yard_exp_brackets_254 -> KW_READCOMMITTED
1164 yard_exp_brackets_254 -> KW_PAGLOCK
1165 yard_exp_brackets_254 -> KW_NOWAIT
1166 yard_exp_brackets_254 -> KW_NOLOCK
1167 yard_exp_brackets_254 -> KW_HOLDLOCK
1168 yard_exp_brackets_254 -> KW_FASTFIRSTROW
1169 yard_exp_brackets_254 -> KW_INDEX LPAREN yard_rule_comma_list_7 RPAREN
1170 yard_exp_brackets_256 -> KW_ROWS
1171 yard_exp_brackets_256 -> KW_PERCENT
1172 yard_exp_brackets_257 -> KW_REPEATABLE sql_expr
1173 yard_exp_brackets_258 -> yard_exp_brackets_420 yard_opt_142
1174 yard_exp_brackets_258 -> KW_INNER
1175 yard_exp_brackets_259 -> KW_WHEN sql_expr KW_THEN sql_expr
1176 yard_exp_brackets_260 -> KW_ELSE sql_expr
1177 yard_exp_brackets_261 -> COMMA table_constraint_create_table
1178 yard_exp_brackets_262 -> computed_column_definition
1179 yard_exp_brackets_262 -> column_definition_create_table
1180 yard_exp_brackets_263 -> COMMA yard_exp_brackets_421
1181 yard_exp_brackets_264 -> IDENT DOT yard_opt_146 yard_opt_147
1182 yard_exp_brackets_265 -> KW_ON yard_exp_brackets_422
1183 yard_exp_brackets_266 -> KW_TEXTIMAGE_ON yard_exp_brackets_423
1184 yard_exp_brackets_267 -> KW_COLLATE IDENT
1185 yard_exp_brackets_268 -> KW_NOT KW_NULL
1186 yard_exp_brackets_268 -> KW_NULL
1187 yard_exp_brackets_269 -> KW_CONSTRAINT IDENT
1188 yard_exp_brackets_270 -> yard_opt_154 KW_DEFAULT sql_expr
1189 yard_exp_brackets_271 -> LPAREN sql_expr COMMA sql_expr RPAREN
1190 yard_exp_brackets_272 -> KW_NOT KW_FOR KW_REPLICATION
1191 yard_exp_brackets_273 -> KW_IDENTITY yard_opt_156 yard_opt_157
1192 yard_exp_brackets_274 -> KW_CHECK yard_opt_167 sql_expr
1193 yard_exp_brackets_274 -> yard_opt_163 KW_REFERENCES yard_opt_164 IDENT yard_opt_165 yard_many_14 yard_opt_166
1194 yard_exp_brackets_274 -> yard_exp_brackets_424 yard_opt_160 yard_opt_161 yard_opt_162
1195 yard_exp_brackets_275 -> KW_CONSTRAINT IDENT
1196 yard_exp_brackets_276 -> KW_NONCLUSTERED
1197 yard_exp_brackets_276 -> KW_CLUSTERED
1198 yard_exp_brackets_277 -> KW_WITH LPAREN yard_rule_comma_list_65 RPAREN
1199 yard_exp_brackets_277 -> KW_WITH KW_FILLFACTOR OP_EQ DEC_NUMBER
1200 yard_exp_brackets_278 -> COMMA index_option
1201 yard_exp_brackets_279 -> KW_ON yard_exp_brackets_425
1202 yard_exp_brackets_280 -> KW_FOREIGN KW_KEY
1203 yard_exp_brackets_281 -> IDENT DOT
1204 yard_exp_brackets_282 -> LPAREN IDENT RPAREN
1205 yard_exp_brackets_283 -> KW_NOT KW_FOR KW_REPLICATION
1206 yard_exp_brackets_284 -> KW_NOT KW_FOR KW_REPLICATION
1207 yard_exp_brackets_285 -> KW_NOT KW_NULL
1208 yard_exp_brackets_286 -> KW_PERSISTED yard_opt_169
1209 yard_exp_brackets_287 -> KW_CONSTRAINT IDENT
1210 yard_exp_brackets_288 -> KW_NONCLUSTERED
1211 yard_exp_brackets_288 -> KW_CLUSTERED
1212 yard_exp_brackets_289 -> KW_WITH LPAREN yard_rule_comma_list_65 RPAREN
1213 yard_exp_brackets_289 -> KW_WITH KW_FILLFACTOR OP_EQ DEC_NUMBER
1214 yard_exp_brackets_290 -> KW_ON yard_exp_brackets_426
1215 yard_exp_brackets_291 -> KW_FOREIGN KW_KEY
1216 yard_exp_brackets_292 -> LPAREN IDENT RPAREN
1217 yard_exp_brackets_293 -> KW_NOT KW_FOR KW_REPLICATION
1218 yard_exp_brackets_294 -> KW_NOT KW_FOR KW_REPLICATION
1219 yard_exp_brackets_295 -> KW_CHECK yard_opt_178 sql_expr
1220 yard_exp_brackets_295 -> yard_opt_175 KW_REFERENCES IDENT yard_opt_176 yard_many_15 yard_opt_177
1221 yard_exp_brackets_295 -> yard_opt_171 yard_exp_brackets_427 yard_opt_172 yard_opt_173 yard_opt_174
1222 yard_exp_brackets_296 -> KW_CHECK yard_opt_186 sql_expr
1223 yard_exp_brackets_296 -> KW_FOREIGN KW_KEY LPAREN yard_rule_comma_list_71 RPAREN KW_REFERENCES IDENT yard_opt_184 yard_many_16 yard_opt_185
1224 yard_exp_brackets_296 -> yard_exp_brackets_428 yard_opt_180 LPAREN yard_rule_comma_list_68 RPAREN yard_opt_182 yard_opt_183
1225 yard_exp_brackets_297 -> IDENT yard_opt_181
1226 yard_exp_brackets_298 -> COMMA yard_exp_brackets_429
1227 yard_exp_brackets_299 -> COMMA IDENT
1228 yard_exp_brackets_300 -> KW_CONSTRAINT IDENT
1229 yard_exp_brackets_301 -> KW_NONCLUSTERED
1230 yard_exp_brackets_301 -> KW_CLUSTERED
1231 yard_exp_brackets_302 -> KW_DESC
1232 yard_exp_brackets_302 -> KW_ASC
1233 yard_exp_brackets_303 -> KW_WITH LPAREN yard_rule_comma_list_65 RPAREN
1234 yard_exp_brackets_303 -> KW_WITH KW_FILLFACTOR OP_EQ DEC_NUMBER
1235 yard_exp_brackets_304 -> KW_ON yard_exp_brackets_430
1236 yard_exp_brackets_305 -> LPAREN yard_rule_comma_list_71 RPAREN
1237 yard_exp_brackets_306 -> KW_NOT KW_FOR KW_REPLICATION
1238 yard_exp_brackets_307 -> KW_NOT KW_FOR KW_REPLICATION
1239 yard_exp_brackets_308 -> KW_UPDATE
1240 yard_exp_brackets_308 -> KW_DELETE
1241 yard_exp_brackets_309 -> KW_ALLOW_PAGE_LOCKS OP_EQ yard_exp_brackets_435
1242 yard_exp_brackets_309 -> KW_ALLOW_ROW_LOCKS OP_EQ yard_exp_brackets_434
1243 yard_exp_brackets_309 -> KW_STATISTICS_NORECOMPUTE OP_EQ yard_exp_brackets_433
1244 yard_exp_brackets_309 -> KW_IGNORE_DUP_KEY OP_EQ yard_exp_brackets_432
1245 yard_exp_brackets_309 -> KW_FILLFACTOR OP_EQ DEC_NUMBER
1246 yard_exp_brackets_309 -> KW_PAD_INDEX OP_EQ yard_exp_brackets_431
1247 yard_exp_brackets_310 -> rowset_function yard_opt_191
1248 yard_exp_brackets_310 -> object
1249 yard_exp_brackets_311 -> yard_opt_192 yard_opt_193 yard_opt_194
1250 yard_exp_brackets_312 -> KW_WITH yard_rule_comma_list_20
1251 yard_exp_brackets_313 -> KW_TOP sql_expr yard_opt_189
1252 yard_exp_brackets_314 -> KW_WITH LPAREN yard_many_17 RPAREN
1253 yard_exp_brackets_315 -> LPAREN yard_rule_comma_list_74 RPAREN
1254 yard_exp_brackets_316 -> COMMA LOCALVAR
1255 yard_exp_brackets_317 -> KW_DEFAULT KW_VALUES
1256 yard_exp_brackets_317 -> batch_body
1257 yard_exp_brackets_317 -> KW_VALUES LPAREN yard_rule_comma_list_77 RPAREN
1258 yard_exp_brackets_318 -> sql_expr
1259 yard_exp_brackets_318 -> KW_NULL
1260 yard_exp_brackets_318 -> KW_DEFAULT
1261 yard_exp_brackets_319 -> COMMA yard_exp_brackets_436
1262 yard_exp_brackets_320 -> IDENT
1263 yard_exp_brackets_320 -> LOCALVAR
1264 yard_exp_brackets_321 -> LPAREN yard_rule_comma_list_74 RPAREN
1265 yard_exp_brackets_322 -> KW_OUTPUT dml_select_list
1266 yard_exp_brackets_323 -> sql_expr
1267 yard_exp_brackets_323 -> ident
1268 yard_exp_brackets_324 -> yard_exp_brackets_437 yard_opt_200
1269 yard_exp_brackets_325 -> COMMA yard_exp_brackets_438
1270 yard_exp_brackets_326 -> yard_opt_199 IDENT
1271 yard_exp_brackets_327 -> yard_opt_201 IDENT
1272 yard_exp_brackets_330 -> IDENT DOT
1273 yard_exp_brackets_330 -> IDENT DOT yard_opt_203 DOT
1274 yard_exp_brackets_330 -> IDENT DOT IDENT DOT IDENT DOT
1275 yard_exp_brackets_331 -> KW_TRANSACTION
1276 yard_exp_brackets_331 -> KW_TRAN
1277 yard_exp_brackets_332 -> KW_WITH KW_MARK yard_opt_206
1278 yard_exp_brackets_333 -> yard_exp_brackets_439 yard_opt_205
1279 yard_exp_brackets_334 -> KW_TRANSACTION
1280 yard_exp_brackets_334 -> KW_TRAN
1281 yard_exp_brackets_335 -> LOCALVAR
1282 yard_exp_brackets_335 -> IDENT
1283 yard_exp_brackets_336 -> create_object DOT IDENT LPAREN yard_rule_comma_list_87 RPAREN
1284 yard_exp_brackets_336 -> yard_exp_brackets_440
1285 yard_exp_brackets_337 -> COMMA yard_opt_213
1286 yard_exp_brackets_338 -> yard_opt_218 sql_expr
1287 yard_exp_brackets_339 -> COMMA yard_exp_brackets_441
1288 yard_exp_brackets_340 -> KW_EXECUTE
1289 yard_exp_brackets_340 -> KW_EXEC
1290 yard_exp_brackets_341 -> LOCALVAR OP_EQ
1291 yard_exp_brackets_342 -> SEMI DEC_NUMBER
1292 yard_exp_brackets_343 -> LOCALVAR OP_EQ
1293 yard_exp_brackets_344 -> KW_OUT
1294 yard_exp_brackets_344 -> KW_OUTPUT
1295 yard_exp_brackets_345 -> yard_opt_214 yard_exp_brackets_442
1296 yard_exp_brackets_346 -> KW_WITH KW_RECOMPILE
1297 yard_exp_brackets_347 -> LOCALVAR OP_EQ
1298 yard_exp_brackets_348 -> KW_EXECUTE
1299 yard_exp_brackets_348 -> KW_EXEC
1300 yard_exp_brackets_349 -> yard_exp_brackets_443 yard_opt_219
1301 yard_exp_brackets_350 -> yard_exp_brackets_444 yard_opt_219
1302 yard_exp_brackets_351 -> KW_AS yard_exp_brackets_445 OP_EQ STRING_CONST
1303 yard_exp_brackets_352 -> KW_EXECUTE
1304 yard_exp_brackets_352 -> KW_EXEC
1305 yard_exp_brackets_353 -> STRING_CONST yard_opt_222 yard_opt_223
1306 yard_exp_brackets_353 -> LOCALVAR
1307 yard_exp_brackets_354 -> STRING_CONST yard_opt_222 yard_opt_223
1308 yard_exp_brackets_354 -> LOCALVAR
1309 yard_exp_brackets_355 -> COMMA sql_expr yard_opt_225
1310 yard_exp_brackets_356 -> COMMA sql_expr yard_opt_225
1311 yard_exp_brackets_357 -> KW_AS yard_exp_brackets_446 OP_EQ STRING_CONST
1312 yard_exp_brackets_358 -> KW_AT full_ident
1313 yard_exp_brackets_359 -> KW_MAX
1314 yard_exp_brackets_359 -> DEC_NUMBER
1315 yard_exp_brackets_360 -> KW_EXECUTE
1316 yard_exp_brackets_360 -> KW_EXEC
1317 yard_exp_brackets_361 -> ident OP_EQ STRING_CONST
1318 yard_exp_brackets_361 -> KW_OWNER
1319 yard_exp_brackets_361 -> KW_SELF
1320 yard_exp_brackets_361 -> KW_CALLER
1321 yard_exp_brackets_362 -> OP_LT OP_MT
1322 yard_exp_brackets_363 -> OP_LT OP_EQ
1323 yard_exp_brackets_364 -> OP_MT OP_EQ
1324 yard_exp_brackets_365 -> OP_LT OP_GT
1325 yard_exp_brackets_366 -> LOCALVAR
1326 yard_exp_brackets_366 -> yard_exp_brackets_447
1327 yard_exp_brackets_367 -> KW_RELATIVE yard_exp_brackets_449
1328 yard_exp_brackets_367 -> KW_ABSOLUTE yard_exp_brackets_448
1329 yard_exp_brackets_367 -> KW_LAST
1330 yard_exp_brackets_367 -> KW_FIRST
1331 yard_exp_brackets_367 -> KW_PRIOR
1332 yard_exp_brackets_367 -> KW_NEXT
1333 yard_exp_brackets_368 -> yard_opt_236 KW_FROM
1334 yard_exp_brackets_369 -> KW_INTO yard_rule_comma_list_74
1335 yard_exp_brackets_370 -> OP_TILDA
1336 yard_exp_brackets_370 -> OP_MINUS_EQ
1337 yard_exp_brackets_370 -> OP_PLUS_EQ
1338 yard_exp_brackets_370 -> KW_NOT
1339 yard_exp_brackets_371 -> KW_OR
1340 yard_exp_brackets_371 -> KW_AND
1341 yard_exp_brackets_372 -> sql_expr
1342 yard_exp_brackets_372 -> sql_datatype
1343 yard_exp_brackets_373 -> COMMA yard_exp_brackets_450
1344 yard_exp_brackets_374 -> KW_XML
1345 yard_exp_brackets_374 -> KW_TIME
1346 yard_exp_brackets_374 -> KW_PROFILE
1347 yard_exp_brackets_374 -> KW_IO
1348 yard_exp_brackets_375 -> DOUBLE_COLON
1349 yard_exp_brackets_375 -> DOT
1350 yard_exp_brackets_376 -> KW_CURSOR yard_opt_7 yard_opt_8 yard_opt_9 yard_opt_10 KW_FOR select_stmnt yard_opt_11
1351 yard_exp_brackets_377 -> KW_UPDATE yard_opt_12
1352 yard_exp_brackets_377 -> KW_EAD ONLY
1353 yard_exp_brackets_378 -> STRING_CONST
1354 yard_exp_brackets_378 -> DEC_NUMBER
1355 yard_exp_brackets_378 -> KW_NULL
1356 yard_exp_brackets_379 -> KW_SETERROR
1357 yard_exp_brackets_379 -> KW_NOWAIT
1358 yard_exp_brackets_379 -> KW_LOG
1359 yard_exp_brackets_380 -> ident yard_opt_38 table_type_definition
1360 yard_exp_brackets_380 -> ident KW_CURSOR yard_opt_35 yard_opt_36 yard_opt_37 sql_expr
1361 yard_exp_brackets_380 -> LOCALVAR yard_opt_34 sql_datatype
1362 yard_exp_brackets_381 -> KW_AS sql_expr
1363 yard_exp_brackets_382 -> KW_UNIQUE
1364 yard_exp_brackets_382 -> KW_PRIMARY KW_KEY
1365 yard_exp_brackets_383 -> DEC_NUMBER
1366 yard_exp_brackets_383 -> ident
1367 yard_exp_brackets_384 -> yard_exp_brackets_451 yard_opt_50
1368 yard_exp_brackets_385 -> KW_SUM
1369 yard_exp_brackets_385 -> KW_MIN
1370 yard_exp_brackets_385 -> KW_MAX
1371 yard_exp_brackets_385 -> KW_COUNT
1372 yard_exp_brackets_385 -> KW_AVG
1373 yard_exp_brackets_386 -> yard_exp_brackets_452 sql_expr
1374 yard_exp_brackets_387 -> xml
1375 yard_exp_brackets_387 -> KW_BROWSE
1376 yard_exp_brackets_388 -> KW_AUTO
1377 yard_exp_brackets_388 -> KW_RAW yard_opt_57
1378 yard_exp_brackets_389 -> KW_XMLSCHEMA yard_opt_60
1379 yard_exp_brackets_389 -> KW_XMLDATA
1380 yard_exp_brackets_390 -> KW_ORDER
1381 yard_exp_brackets_390 -> KW_HASH
1382 yard_exp_brackets_391 -> KW_MERGE
1383 yard_exp_brackets_391 -> KW_HASH
1384 yard_exp_brackets_391 -> KW_CONCAT
1385 yard_exp_brackets_392 -> KW_HASH
1386 yard_exp_brackets_392 -> KW_MERGE
1387 yard_exp_brackets_392 -> KW_LOOP
1388 yard_exp_brackets_393 -> KW_FORCED
1389 yard_exp_brackets_393 -> KW_SIMPLE
1390 yard_exp_brackets_394 -> LOCALVAR OP_EQ STRING_CONST
1391 yard_exp_brackets_395 -> KW_INTERSECT
1392 yard_exp_brackets_395 -> KW_EXCEPT
1393 yard_exp_brackets_395 -> KW_UNION yard_opt_76
1394 yard_exp_brackets_396 -> LPAREN query_expression RPAREN
1395 yard_exp_brackets_397 -> KW_ROLLUP
1396 yard_exp_brackets_397 -> KW_CUBE
1397 yard_exp_brackets_398 -> sql_expr yard_opt_90
1398 yard_exp_brackets_398 -> yard_opt_88 yard_exp_brackets_453 yard_opt_89
1399 yard_exp_brackets_399 -> ident OP_EQ sql_expr
1400 yard_exp_brackets_399 -> yard_exp_brackets_454
1401 yard_exp_brackets_399 -> full_ident DOT STAR
1402 yard_exp_brackets_399 -> STAR
1403 yard_exp_brackets_400 -> DOUBLE_COLON
1404 yard_exp_brackets_400 -> DOT
1405 yard_exp_brackets_401 -> ident LPAREN yard_rule_comma_list_11 RPAREN
1406 yard_exp_brackets_401 -> yard_exp_brackets_455
1407 yard_exp_brackets_402 -> sql_expr yard_opt_97
1408 yard_exp_brackets_403 -> KW_OR
1409 yard_exp_brackets_403 -> KW_AND
1410 yard_exp_brackets_404 -> LPAREN search_condition RPAREN
1411 yard_exp_brackets_404 -> predicate
1412 yard_exp_brackets_405 -> STRING_CONST
1413 yard_exp_brackets_405 -> dec_num
1414 yard_exp_brackets_406 -> STRING_CONST
1415 yard_exp_brackets_406 -> dec_num
1416 yard_exp_brackets_407 -> rowset_function yard_opt_109 yard_opt_111
1417 yard_exp_brackets_408 -> STRING_CONST
1418 yard_exp_brackets_408 -> STRING_CONST SEMI STRING_CONST SEMI STRING_CONST
1419 yard_exp_brackets_409 -> STRING_CONST
1420 yard_exp_brackets_409 -> yard_opt_121 yard_opt_122 full_ident
1421 yard_exp_brackets_410 -> KW_SINGLE_NCLOB
1422 yard_exp_brackets_410 -> KW_SINGLE_CLOB
1423 yard_exp_brackets_410 -> KW_SINGLE_BLOB
1424 yard_exp_brackets_410 -> KW_FORMATFILE OP_EQ STRING_CONST yard_opt_123
1425 yard_exp_brackets_411 -> DEC_NUMBER
1426 yard_exp_brackets_411 -> STRING_CONST
1427 yard_exp_brackets_412 -> DEC_NUMBER
1428 yard_exp_brackets_412 -> STRING_CONST
1429 yard_exp_brackets_413 -> yard_exp_brackets_458
1430 yard_exp_brackets_413 -> yard_exp_brackets_457
1431 yard_exp_brackets_413 -> yard_exp_brackets_456
1432 yard_exp_brackets_414 -> OP_TILDA
1433 yard_exp_brackets_414 -> KW_NEAR
1434 yard_exp_brackets_415 -> prefix_term
1435 yard_exp_brackets_415 -> simple_term
1436 yard_exp_brackets_416 -> OP_TILDA
1437 yard_exp_brackets_416 -> KW_NEAR
1438 yard_exp_brackets_417 -> prefix_term
1439 yard_exp_brackets_417 -> simple_term
1440 yard_exp_brackets_418 -> proximity_term
1441 yard_exp_brackets_418 -> generation_term
1442 yard_exp_brackets_418 -> prefix_term
1443 yard_exp_brackets_418 -> simple_term
1444 yard_exp_brackets_419 -> yard_exp_brackets_459 yard_opt_134
1445 yard_exp_brackets_420 -> yard_exp_brackets_460 yard_opt_141
1446 yard_exp_brackets_421 -> computed_column_definition
1447 yard_exp_brackets_421 -> column_definition_create_table
1448 yard_exp_brackets_422 -> KW_DEFAULT
1449 yard_exp_brackets_422 -> IDENT
1450 yard_exp_brackets_422 -> IDENT LPAREN IDENT RPAREN
1451 yard_exp_brackets_423 -> KW_DEFAULT
1452 yard_exp_brackets_423 -> IDENT
1453 yard_exp_brackets_424 -> KW_UNIQUE
1454 yard_exp_brackets_424 -> KW_PRIMARY KW_KEY
1455 yard_exp_brackets_425 -> KW_DEFAULT
1456 yard_exp_brackets_425 -> IDENT
1457 yard_exp_brackets_425 -> IDENT LPAREN IDENT RPAREN
1458 yard_exp_brackets_426 -> KW_DEFAULT
1459 yard_exp_brackets_426 -> IDENT
1460 yard_exp_brackets_426 -> IDENT LPAREN IDENT RPAREN
1461 yard_exp_brackets_427 -> KW_UNIQUE
1462 yard_exp_brackets_427 -> KW_PRIMARY KW_KEY
1463 yard_exp_brackets_428 -> KW_UNIQUE
1464 yard_exp_brackets_428 -> KW_PRIMARY KW_KEY
1465 yard_exp_brackets_429 -> IDENT yard_opt_181
1466 yard_exp_brackets_430 -> KW_DEFAULT
1467 yard_exp_brackets_430 -> IDENT
1468 yard_exp_brackets_430 -> IDENT LPAREN IDENT RPAREN
1469 yard_exp_brackets_431 -> KW_OFF
1470 yard_exp_brackets_431 -> KW_ON
1471 yard_exp_brackets_432 -> KW_OFF
1472 yard_exp_brackets_432 -> KW_ON
1473 yard_exp_brackets_433 -> KW_OFF
1474 yard_exp_brackets_433 -> KW_ON
1475 yard_exp_brackets_434 -> KW_OFF
1476 yard_exp_brackets_434 -> KW_ON
1477 yard_exp_brackets_435 -> KW_OFF
1478 yard_exp_brackets_435 -> KW_ON
1479 yard_exp_brackets_436 -> sql_expr
1480 yard_exp_brackets_436 -> KW_NULL
1481 yard_exp_brackets_436 -> KW_DEFAULT
1482 yard_exp_brackets_437 -> sql_expr
1483 yard_exp_brackets_437 -> ident
1484 yard_exp_brackets_438 -> yard_exp_brackets_461 yard_opt_200
1485 yard_exp_brackets_439 -> LOCALVAR
1486 yard_exp_brackets_439 -> ident
1487 yard_exp_brackets_440 -> yard_opt_211 yard_exp_brackets_462 yard_rule_brace_opt_86 yard_opt_217
1488 yard_exp_brackets_441 -> yard_opt_218 sql_expr
1489 yard_exp_brackets_442 -> yard_opt_216
1490 yard_exp_brackets_442 -> LOCALVAR yard_opt_215
1491 yard_exp_brackets_442 -> sql_expr
1492 yard_exp_brackets_443 -> STRING_CONST
1493 yard_exp_brackets_443 -> LOCALVAR
1494 yard_exp_brackets_444 -> STRING_CONST
1495 yard_exp_brackets_444 -> LOCALVAR
1496 yard_exp_brackets_445 -> KW_USER
1497 yard_exp_brackets_445 -> KW_LOGIN
1498 yard_exp_brackets_446 -> KW_USER
1499 yard_exp_brackets_446 -> KW_LOGIN
1500 yard_exp_brackets_447 -> yard_opt_237 IDENT
1501 yard_exp_brackets_448 -> LOCALVAR
1502 yard_exp_brackets_448 -> dec_num
1503 yard_exp_brackets_449 -> LOCALVAR
1504 yard_exp_brackets_449 -> dec_num
1505 yard_exp_brackets_450 -> sql_expr
1506 yard_exp_brackets_450 -> sql_datatype
1507 yard_exp_brackets_451 -> DEC_NUMBER
1508 yard_exp_brackets_451 -> ident
1509 yard_exp_brackets_452 -> KW_SUM
1510 yard_exp_brackets_452 -> KW_MIN
1511 yard_exp_brackets_452 -> KW_MAX
1512 yard_exp_brackets_452 -> KW_COUNT
1513 yard_exp_brackets_452 -> KW_AVG
1514 yard_exp_brackets_453 -> KW_ROWGUID
1515 yard_exp_brackets_453 -> KW_IDENTITY
1516 yard_exp_brackets_453 -> ident
1517 yard_exp_brackets_454 -> sql_expr yard_opt_90
1518 yard_exp_brackets_454 -> yard_opt_88 yard_exp_brackets_463 yard_opt_89
1519 yard_exp_brackets_455 -> ident
1520 yard_exp_brackets_455 -> STRING_CONST
1521 yard_exp_brackets_456 -> EMPTY
1522 yard_exp_brackets_456 -> KW_AND
1523 yard_exp_brackets_457 -> EMPTY
1524 yard_exp_brackets_457 -> KW_AND KW_NOT
1525 yard_exp_brackets_458 -> EMPTY
1526 yard_exp_brackets_458 -> KW_OR
1527 yard_exp_brackets_459 -> proximity_term
1528 yard_exp_brackets_459 -> generation_term
1529 yard_exp_brackets_459 -> prefix_term
1530 yard_exp_brackets_459 -> simple_term
1531 yard_exp_brackets_460 -> KW_FULL
1532 yard_exp_brackets_460 -> KW_RIGHT
1533 yard_exp_brackets_460 -> KW_LEFT
1534 yard_exp_brackets_461 -> sql_expr
1535 yard_exp_brackets_461 -> ident
1536 yard_exp_brackets_462 -> LOCALVAR
1537 yard_exp_brackets_462 -> full_ident yard_opt_212
1538 yard_exp_brackets_463 -> KW_ROWGUID
1539 yard_exp_brackets_463 -> KW_IDENTITY
1540 yard_exp_brackets_463 -> ident
*)
module Yard.Examples.MSParser
open Yard.Generators.GLL.AST
open Yard.Generators.GLL.Parser

open Microsoft.FSharp.Text.Lexing
open Yard.Utils.SourceText

type Token =
    | COMMA of SourceText
    | DEC_NUMBER of SourceText
    | DOT of SourceText
    | DOUBLE_COLON of SourceText
    | EMPTY of SourceText
    | EOF of SourceText
    | GLOBALVAR of SourceText
    | IDENT of SourceText
    | KW_ABSENT of SourceText
    | KW_ABSOLUTE of SourceText
    | KW_ACTION of SourceText
    | KW_ALL of SourceText
    | KW_ALLOW_PAGE_LOCKS of SourceText
    | KW_ALLOW_ROW_LOCKS of SourceText
    | KW_AND of SourceText
    | KW_ANSI_DEFAULTS of SourceText
    | KW_ANSI_NULLS of SourceText
    | KW_ANSI_NULL_DFLT of SourceText
    | KW_ANSI_PADDING of SourceText
    | KW_ANSI_WARNINGS of SourceText
    | KW_ANY of SourceText
    | KW_AS of SourceText
    | KW_ASC of SourceText
    | KW_AT of SourceText
    | KW_AUTO of SourceText
    | KW_AVG of SourceText
    | KW_BASE64 of SourceText
    | KW_BEGIN of SourceText
    | KW_BETWEEN of SourceText
    | KW_BIGINT of SourceText
    | KW_BINARY of SourceText
    | KW_BIT of SourceText
    | KW_BROWSE of SourceText
    | KW_BULK of SourceText
    | KW_BY of SourceText
    | KW_CALLER of SourceText
    | KW_CASCADE of SourceText
    | KW_CASE of SourceText
    | KW_CHAR of SourceText
    | KW_CHECK of SourceText
    | KW_CLASS of SourceText
    | KW_CLUSTERED of SourceText
    | KW_CODEPAGE of SourceText
    | KW_COLLATE of SourceText
    | KW_COMMIT of SourceText
    | KW_COMPUTE of SourceText
    | KW_CONCAT of SourceText
    | KW_CONCAT_NULL_YIELDS_NULL of SourceText
    | KW_CONSTRAINT of SourceText
    | KW_CONTAINSTABLE of SourceText
    | KW_COUNT of SourceText
    | KW_CREATE of SourceText
    | KW_CROSS of SourceText
    | KW_CUBE of SourceText
    | KW_CURSOR of SourceText
    | KW_CURSOR_CLOSE_ON_COMMIT of SourceText
    | KW_DATETIME of SourceText
    | KW_DECIMAL of SourceText
    | KW_DECLARE of SourceText
    | KW_DEFAULT of SourceText
    | KW_DELETE of SourceText
    | KW_DENSE_RANK of SourceText
    | KW_DESC of SourceText
    | KW_DISTINCT of SourceText
    | KW_DROP of SourceText
    | KW_DYNAMIC of SourceText
    | KW_EAD of SourceText
    | KW_ELEMENTS of SourceText
    | KW_ELSE of SourceText
    | KW_END of SourceText
    | KW_ERRORFILE of SourceText
    | KW_ESCAPE of SourceText
    | KW_EXCEPT of SourceText
    | KW_EXEC of SourceText
    | KW_EXECUTE of SourceText
    | KW_EXISTS of SourceText
    | KW_EXPAND of SourceText
    | KW_EXPLICIT of SourceText
    | KW_FAST of SourceText
    | KW_FASTFIRSTROW of SourceText
    | KW_FAST_FORWARD of SourceText
    | KW_FETCH of SourceText
    | KW_FILLFACTOR of SourceText
    | KW_FIRST of SourceText
    | KW_FIRSTROW of SourceText
    | KW_FLOAT of SourceText
    | KW_FMTONLY of SourceText
    | KW_FOR of SourceText
    | KW_FORCE of SourceText
    | KW_FORCED of SourceText
    | KW_FORCEPLAN of SourceText
    | KW_FOREIGN of SourceText
    | KW_FORMATFILE of SourceText
    | KW_FORMSOF of SourceText
    | KW_FORWARD_ONLY of SourceText
    | KW_FREETEXTTABLE of SourceText
    | KW_FROM of SourceText
    | KW_FULL of SourceText
    | KW_GLOBAL of SourceText
    | KW_GO of SourceText
    | KW_GROUP of SourceText
    | KW_HASH of SourceText
    | KW_HAVING of SourceText
    | KW_HOLDLOCK of SourceText
    | KW_IDENTITY of SourceText
    | KW_IF of SourceText
    | KW_IGNORE_DUP_KEY of SourceText
    | KW_IMPLICIT_TRANSACTIONS of SourceText
    | KW_IN of SourceText
    | KW_INDEX of SourceText
    | KW_INFLECTIONAL of SourceText
    | KW_INNER of SourceText
    | KW_INSERT of SourceText
    | KW_INT of SourceText
    | KW_INTERSECT of SourceText
    | KW_INTO of SourceText
    | KW_IO of SourceText
    | KW_IS of SourceText
    | KW_ISABOUT of SourceText
    | KW_IS_MEMBER of SourceText
    | KW_JOIN of SourceText
    | KW_KEEP of SourceText
    | KW_KEEPFIXED of SourceText
    | KW_KEY of SourceText
    | KW_KEYSET of SourceText
    | KW_LANGUAGE of SourceText
    | KW_LAST of SourceText
    | KW_LASTROW of SourceText
    | KW_LEFT of SourceText
    | KW_LIKE of SourceText
    | KW_LOCAL of SourceText
    | KW_LOG of SourceText
    | KW_LOGIN of SourceText
    | KW_LOOP of SourceText
    | KW_LOWER of SourceText
    | KW_MARK of SourceText
    | KW_MAX of SourceText
    | KW_MAXDOP of SourceText
    | KW_MAXERRORS of SourceText
    | KW_MAXRECURSION of SourceText
    | KW_MDW_CONTROL of SourceText
    | KW_MERGE of SourceText
    | KW_MIN of SourceText
    | KW_MONEY of SourceText
    | KW_NCHAR of SourceText
    | KW_NEAR of SourceText
    | KW_NEXT of SourceText
    | KW_NO of SourceText
    | KW_NOCOUNT of SourceText
    | KW_NOEXEC of SourceText
    | KW_NOEXPAND of SourceText
    | KW_NOLOCK of SourceText
    | KW_NONCLUSTERED of SourceText
    | KW_NOT of SourceText
    | KW_NOWAIT of SourceText
    | KW_NTILE of SourceText
    | KW_NULL of SourceText
    | KW_NUMERIC of SourceText
    | KW_NUMERIC_ROUNDABORT of SourceText
    | KW_NVARCHAR of SourceText
    | KW_OF of SourceText
    | KW_OFF of SourceText
    | KW_ON of SourceText
    | KW_OPENDATASOURCE of SourceText
    | KW_OPENQUERY of SourceText
    | KW_OPENROWSET of SourceText
    | KW_OPTIMISTIC of SourceText
    | KW_OPTIMIZE of SourceText
    | KW_OPTION of SourceText
    | KW_OR of SourceText
    | KW_ORDER of SourceText
    | KW_OUT of SourceText
    | KW_OUTER of SourceText
    | KW_OUTPUT of SourceText
    | KW_OVER of SourceText
    | KW_OWNER of SourceText
    | KW_PAD_INDEX of SourceText
    | KW_PAGLOCK of SourceText
    | KW_PARAMETERIZATION of SourceText
    | KW_PARSEONLY of SourceText
    | KW_PARTITION of SourceText
    | KW_PATH of SourceText
    | KW_PERCENT of SourceText
    | KW_PERSISTED of SourceText
    | KW_PIVOT of SourceText
    | KW_PLAN of SourceText
    | KW_PRIMARY of SourceText
    | KW_PRINT of SourceText
    | KW_PRIOR of SourceText
    | KW_PROC of SourceText
    | KW_PROCEDURE of SourceText
    | KW_PROFILE of SourceText
    | KW_QUOTED_IDENTIFIER of SourceText
    | KW_RAISERROR of SourceText
    | KW_RANK of SourceText
    | KW_RAW of SourceText
    | KW_READCOMMITTED of SourceText
    | KW_READCOMMITTEDLOCK of SourceText
    | KW_READONLY of SourceText
    | KW_READPAST of SourceText
    | KW_READUNCOMMITTED of SourceText
    | KW_READ_ONLY of SourceText
    | KW_REAL of SourceText
    | KW_RECOMPILE of SourceText
    | KW_REFERENCES of SourceText
    | KW_RELATIVE of SourceText
    | KW_REMOTE of SourceText
    | KW_REMOTE_PROC_TRANSACTIONS of SourceText
    | KW_REPEATABLE of SourceText
    | KW_REPEATABLEREAD of SourceText
    | KW_REPLICATION of SourceText
    | KW_RETURN of SourceText
    | KW_RIGHT of SourceText
    | KW_ROBUST of SourceText
    | KW_ROLLBACK of SourceText
    | KW_ROLLUP of SourceText
    | KW_ROOT of SourceText
    | KW_ROWGUID of SourceText
    | KW_ROWGUIDCOL of SourceText
    | KW_ROWLOCK of SourceText
    | KW_ROWS of SourceText
    | KW_ROWS_PER_BATCH of SourceText
    | KW_ROW_NUMBER of SourceText
    | KW_SCALAR_DATA_TYPE of SourceText
    | KW_SCROLL of SourceText
    | KW_SCROLL_LOCKS of SourceText
    | KW_SELECT of SourceText
    | KW_SELF of SourceText
    | KW_SERIALIZABLE of SourceText
    | KW_SET of SourceText
    | KW_SETERROR of SourceText
    | KW_SHOWPLAN_ALL of SourceText
    | KW_SHOWPLAN_TEXT of SourceText
    | KW_SHOWPLAN_XML of SourceText
    | KW_SIMPLE of SourceText
    | KW_SINGLE_BLOB of SourceText
    | KW_SINGLE_CLOB of SourceText
    | KW_SINGLE_NCLOB of SourceText
    | KW_SMALLINT of SourceText
    | KW_SMALLMONEY of SourceText
    | KW_SOME of SourceText
    | KW_SQL_VARIANT of SourceText
    | KW_STATE of SourceText
    | KW_STATIC of SourceText
    | KW_STATISTICS of SourceText
    | KW_STATISTICS_NORECOMPUTE of SourceText
    | KW_SUM of SourceText
    | KW_SYSNAME of SourceText
    | KW_SYSTEM of SourceText
    | KW_TABLE of SourceText
    | KW_TABLESAMPLE of SourceText
    | KW_TABLOCK of SourceText
    | KW_TABLOCKX of SourceText
    | KW_TEXTIMAGE_ON of SourceText
    | KW_THEN of SourceText
    | KW_THESAURUS of SourceText
    | KW_TIES of SourceText
    | KW_TIME of SourceText
    | KW_TINYINT of SourceText
    | KW_TOP of SourceText
    | KW_TRAN of SourceText
    | KW_TRANSACTION of SourceText
    | KW_TYPE of SourceText
    | KW_TYPE_WARNING of SourceText
    | KW_UNION of SourceText
    | KW_UNIQUE of SourceText
    | KW_UNPIVOT of SourceText
    | KW_UPDATE of SourceText
    | KW_UPDLOCK of SourceText
    | KW_USE of SourceText
    | KW_USER of SourceText
    | KW_VALUES of SourceText
    | KW_VARBINARY of SourceText
    | KW_VARCHAR of SourceText
    | KW_VARYING of SourceText
    | KW_VIEWS of SourceText
    | KW_WHEN of SourceText
    | KW_WHERE of SourceText
    | KW_WHILE of SourceText
    | KW_WITH of SourceText
    | KW_XACT_ABORT of SourceText
    | KW_XLOCK of SourceText
    | KW_XML of SourceText
    | KW_XMLDATA of SourceText
    | KW_XMLSCHEMA of SourceText
    | KW_XSINIL of SourceText
    | LBRACKET of SourceText
    | LOCALVAR of SourceText
    | LPAREN of SourceText
    | ONLY of SourceText
    | OP_AND_EQ of SourceText
    | OP_DIV of SourceText
    | OP_DIV_EQ of SourceText
    | OP_EQ of SourceText
    | OP_GT of SourceText
    | OP_LT of SourceText
    | OP_MINUS of SourceText
    | OP_MINUS_EQ of SourceText
    | OP_MOD of SourceText
    | OP_MOD_EQ of SourceText
    | OP_MT of SourceText
    | OP_MUL_EQ of SourceText
    | OP_OR_EQ of SourceText
    | OP_PLUS of SourceText
    | OP_PLUS_EQ of SourceText
    | OP_TILDA of SourceText
    | OP_XOR_EQ of SourceText
    | PLUS of SourceText
    | RBRACKET of SourceText
    | RPAREN of SourceText
    | SEMI of SourceText
    | STAR of SourceText
    | STOREDPROCEDURE of SourceText
    | STRING_CONST of SourceText
    | WEIGHT of SourceText

let tokenToNumber = function
    | COMMA(_) -> 807
    | DEC_NUMBER(_) -> 808
    | DOT(_) -> 809
    | DOUBLE_COLON(_) -> 810
    | EMPTY(_) -> 811
    | EOF(_) -> 812
    | GLOBALVAR(_) -> 813
    | IDENT(_) -> 814
    | KW_ABSENT(_) -> 815
    | KW_ABSOLUTE(_) -> 816
    | KW_ACTION(_) -> 817
    | KW_ALL(_) -> 818
    | KW_ALLOW_PAGE_LOCKS(_) -> 819
    | KW_ALLOW_ROW_LOCKS(_) -> 820
    | KW_AND(_) -> 821
    | KW_ANSI_DEFAULTS(_) -> 822
    | KW_ANSI_NULLS(_) -> 823
    | KW_ANSI_NULL_DFLT(_) -> 824
    | KW_ANSI_PADDING(_) -> 825
    | KW_ANSI_WARNINGS(_) -> 826
    | KW_ANY(_) -> 827
    | KW_AS(_) -> 828
    | KW_ASC(_) -> 829
    | KW_AT(_) -> 830
    | KW_AUTO(_) -> 831
    | KW_AVG(_) -> 832
    | KW_BASE64(_) -> 833
    | KW_BEGIN(_) -> 834
    | KW_BETWEEN(_) -> 835
    | KW_BIGINT(_) -> 836
    | KW_BINARY(_) -> 837
    | KW_BIT(_) -> 838
    | KW_BROWSE(_) -> 839
    | KW_BULK(_) -> 840
    | KW_BY(_) -> 841
    | KW_CALLER(_) -> 842
    | KW_CASCADE(_) -> 843
    | KW_CASE(_) -> 844
    | KW_CHAR(_) -> 845
    | KW_CHECK(_) -> 846
    | KW_CLASS(_) -> 847
    | KW_CLUSTERED(_) -> 848
    | KW_CODEPAGE(_) -> 849
    | KW_COLLATE(_) -> 850
    | KW_COMMIT(_) -> 851
    | KW_COMPUTE(_) -> 852
    | KW_CONCAT(_) -> 853
    | KW_CONCAT_NULL_YIELDS_NULL(_) -> 854
    | KW_CONSTRAINT(_) -> 855
    | KW_CONTAINSTABLE(_) -> 856
    | KW_COUNT(_) -> 857
    | KW_CREATE(_) -> 858
    | KW_CROSS(_) -> 859
    | KW_CUBE(_) -> 860
    | KW_CURSOR(_) -> 861
    | KW_CURSOR_CLOSE_ON_COMMIT(_) -> 862
    | KW_DATETIME(_) -> 863
    | KW_DECIMAL(_) -> 864
    | KW_DECLARE(_) -> 865
    | KW_DEFAULT(_) -> 866
    | KW_DELETE(_) -> 867
    | KW_DENSE_RANK(_) -> 868
    | KW_DESC(_) -> 869
    | KW_DISTINCT(_) -> 870
    | KW_DROP(_) -> 871
    | KW_DYNAMIC(_) -> 872
    | KW_EAD(_) -> 873
    | KW_ELEMENTS(_) -> 874
    | KW_ELSE(_) -> 875
    | KW_END(_) -> 876
    | KW_ERRORFILE(_) -> 877
    | KW_ESCAPE(_) -> 878
    | KW_EXCEPT(_) -> 879
    | KW_EXEC(_) -> 880
    | KW_EXECUTE(_) -> 881
    | KW_EXISTS(_) -> 882
    | KW_EXPAND(_) -> 883
    | KW_EXPLICIT(_) -> 884
    | KW_FAST(_) -> 885
    | KW_FASTFIRSTROW(_) -> 886
    | KW_FAST_FORWARD(_) -> 887
    | KW_FETCH(_) -> 888
    | KW_FILLFACTOR(_) -> 889
    | KW_FIRST(_) -> 890
    | KW_FIRSTROW(_) -> 891
    | KW_FLOAT(_) -> 892
    | KW_FMTONLY(_) -> 893
    | KW_FOR(_) -> 894
    | KW_FORCE(_) -> 895
    | KW_FORCED(_) -> 896
    | KW_FORCEPLAN(_) -> 897
    | KW_FOREIGN(_) -> 898
    | KW_FORMATFILE(_) -> 899
    | KW_FORMSOF(_) -> 900
    | KW_FORWARD_ONLY(_) -> 901
    | KW_FREETEXTTABLE(_) -> 902
    | KW_FROM(_) -> 903
    | KW_FULL(_) -> 904
    | KW_GLOBAL(_) -> 905
    | KW_GO(_) -> 906
    | KW_GROUP(_) -> 907
    | KW_HASH(_) -> 908
    | KW_HAVING(_) -> 909
    | KW_HOLDLOCK(_) -> 910
    | KW_IDENTITY(_) -> 911
    | KW_IF(_) -> 912
    | KW_IGNORE_DUP_KEY(_) -> 913
    | KW_IMPLICIT_TRANSACTIONS(_) -> 914
    | KW_IN(_) -> 915
    | KW_INDEX(_) -> 916
    | KW_INFLECTIONAL(_) -> 917
    | KW_INNER(_) -> 918
    | KW_INSERT(_) -> 919
    | KW_INT(_) -> 920
    | KW_INTERSECT(_) -> 921
    | KW_INTO(_) -> 922
    | KW_IO(_) -> 923
    | KW_IS(_) -> 924
    | KW_ISABOUT(_) -> 925
    | KW_IS_MEMBER(_) -> 926
    | KW_JOIN(_) -> 927
    | KW_KEEP(_) -> 928
    | KW_KEEPFIXED(_) -> 929
    | KW_KEY(_) -> 930
    | KW_KEYSET(_) -> 931
    | KW_LANGUAGE(_) -> 932
    | KW_LAST(_) -> 933
    | KW_LASTROW(_) -> 934
    | KW_LEFT(_) -> 935
    | KW_LIKE(_) -> 936
    | KW_LOCAL(_) -> 937
    | KW_LOG(_) -> 938
    | KW_LOGIN(_) -> 939
    | KW_LOOP(_) -> 940
    | KW_LOWER(_) -> 941
    | KW_MARK(_) -> 942
    | KW_MAX(_) -> 943
    | KW_MAXDOP(_) -> 944
    | KW_MAXERRORS(_) -> 945
    | KW_MAXRECURSION(_) -> 946
    | KW_MDW_CONTROL(_) -> 947
    | KW_MERGE(_) -> 948
    | KW_MIN(_) -> 949
    | KW_MONEY(_) -> 950
    | KW_NCHAR(_) -> 951
    | KW_NEAR(_) -> 952
    | KW_NEXT(_) -> 953
    | KW_NO(_) -> 954
    | KW_NOCOUNT(_) -> 955
    | KW_NOEXEC(_) -> 956
    | KW_NOEXPAND(_) -> 957
    | KW_NOLOCK(_) -> 958
    | KW_NONCLUSTERED(_) -> 959
    | KW_NOT(_) -> 960
    | KW_NOWAIT(_) -> 961
    | KW_NTILE(_) -> 962
    | KW_NULL(_) -> 963
    | KW_NUMERIC(_) -> 964
    | KW_NUMERIC_ROUNDABORT(_) -> 965
    | KW_NVARCHAR(_) -> 966
    | KW_OF(_) -> 967
    | KW_OFF(_) -> 968
    | KW_ON(_) -> 969
    | KW_OPENDATASOURCE(_) -> 970
    | KW_OPENQUERY(_) -> 971
    | KW_OPENROWSET(_) -> 972
    | KW_OPTIMISTIC(_) -> 973
    | KW_OPTIMIZE(_) -> 974
    | KW_OPTION(_) -> 975
    | KW_OR(_) -> 976
    | KW_ORDER(_) -> 977
    | KW_OUT(_) -> 978
    | KW_OUTER(_) -> 979
    | KW_OUTPUT(_) -> 980
    | KW_OVER(_) -> 981
    | KW_OWNER(_) -> 982
    | KW_PAD_INDEX(_) -> 983
    | KW_PAGLOCK(_) -> 984
    | KW_PARAMETERIZATION(_) -> 985
    | KW_PARSEONLY(_) -> 986
    | KW_PARTITION(_) -> 987
    | KW_PATH(_) -> 988
    | KW_PERCENT(_) -> 989
    | KW_PERSISTED(_) -> 990
    | KW_PIVOT(_) -> 991
    | KW_PLAN(_) -> 992
    | KW_PRIMARY(_) -> 993
    | KW_PRINT(_) -> 994
    | KW_PRIOR(_) -> 995
    | KW_PROC(_) -> 996
    | KW_PROCEDURE(_) -> 997
    | KW_PROFILE(_) -> 998
    | KW_QUOTED_IDENTIFIER(_) -> 999
    | KW_RAISERROR(_) -> 1000
    | KW_RANK(_) -> 1001
    | KW_RAW(_) -> 1002
    | KW_READCOMMITTED(_) -> 1003
    | KW_READCOMMITTEDLOCK(_) -> 1004
    | KW_READONLY(_) -> 1005
    | KW_READPAST(_) -> 1006
    | KW_READUNCOMMITTED(_) -> 1007
    | KW_READ_ONLY(_) -> 1008
    | KW_REAL(_) -> 1009
    | KW_RECOMPILE(_) -> 1010
    | KW_REFERENCES(_) -> 1011
    | KW_RELATIVE(_) -> 1012
    | KW_REMOTE(_) -> 1013
    | KW_REMOTE_PROC_TRANSACTIONS(_) -> 1014
    | KW_REPEATABLE(_) -> 1015
    | KW_REPEATABLEREAD(_) -> 1016
    | KW_REPLICATION(_) -> 1017
    | KW_RETURN(_) -> 1018
    | KW_RIGHT(_) -> 1019
    | KW_ROBUST(_) -> 1020
    | KW_ROLLBACK(_) -> 1021
    | KW_ROLLUP(_) -> 1022
    | KW_ROOT(_) -> 1023
    | KW_ROWGUID(_) -> 1024
    | KW_ROWGUIDCOL(_) -> 1025
    | KW_ROWLOCK(_) -> 1026
    | KW_ROWS(_) -> 1027
    | KW_ROWS_PER_BATCH(_) -> 1028
    | KW_ROW_NUMBER(_) -> 1029
    | KW_SCALAR_DATA_TYPE(_) -> 1030
    | KW_SCROLL(_) -> 1031
    | KW_SCROLL_LOCKS(_) -> 1032
    | KW_SELECT(_) -> 1033
    | KW_SELF(_) -> 1034
    | KW_SERIALIZABLE(_) -> 1035
    | KW_SET(_) -> 1036
    | KW_SETERROR(_) -> 1037
    | KW_SHOWPLAN_ALL(_) -> 1038
    | KW_SHOWPLAN_TEXT(_) -> 1039
    | KW_SHOWPLAN_XML(_) -> 1040
    | KW_SIMPLE(_) -> 1041
    | KW_SINGLE_BLOB(_) -> 1042
    | KW_SINGLE_CLOB(_) -> 1043
    | KW_SINGLE_NCLOB(_) -> 1044
    | KW_SMALLINT(_) -> 1045
    | KW_SMALLMONEY(_) -> 1046
    | KW_SOME(_) -> 1047
    | KW_SQL_VARIANT(_) -> 1048
    | KW_STATE(_) -> 1049
    | KW_STATIC(_) -> 1050
    | KW_STATISTICS(_) -> 1051
    | KW_STATISTICS_NORECOMPUTE(_) -> 1052
    | KW_SUM(_) -> 1053
    | KW_SYSNAME(_) -> 1054
    | KW_SYSTEM(_) -> 1055
    | KW_TABLE(_) -> 1056
    | KW_TABLESAMPLE(_) -> 1057
    | KW_TABLOCK(_) -> 1058
    | KW_TABLOCKX(_) -> 1059
    | KW_TEXTIMAGE_ON(_) -> 1060
    | KW_THEN(_) -> 1061
    | KW_THESAURUS(_) -> 1062
    | KW_TIES(_) -> 1063
    | KW_TIME(_) -> 1064
    | KW_TINYINT(_) -> 1065
    | KW_TOP(_) -> 1066
    | KW_TRAN(_) -> 1067
    | KW_TRANSACTION(_) -> 1068
    | KW_TYPE(_) -> 1069
    | KW_TYPE_WARNING(_) -> 1070
    | KW_UNION(_) -> 1071
    | KW_UNIQUE(_) -> 1072
    | KW_UNPIVOT(_) -> 1073
    | KW_UPDATE(_) -> 1074
    | KW_UPDLOCK(_) -> 1075
    | KW_USE(_) -> 1076
    | KW_USER(_) -> 1077
    | KW_VALUES(_) -> 1078
    | KW_VARBINARY(_) -> 1079
    | KW_VARCHAR(_) -> 1080
    | KW_VARYING(_) -> 1081
    | KW_VIEWS(_) -> 1082
    | KW_WHEN(_) -> 1083
    | KW_WHERE(_) -> 1084
    | KW_WHILE(_) -> 1085
    | KW_WITH(_) -> 1086
    | KW_XACT_ABORT(_) -> 1087
    | KW_XLOCK(_) -> 1088
    | KW_XML(_) -> 1089
    | KW_XMLDATA(_) -> 1090
    | KW_XMLSCHEMA(_) -> 1091
    | KW_XSINIL(_) -> 1092
    | LBRACKET(_) -> 1093
    | LOCALVAR(_) -> 1094
    | LPAREN(_) -> 1095
    | ONLY(_) -> 1096
    | OP_AND_EQ(_) -> 1097
    | OP_DIV(_) -> 1098
    | OP_DIV_EQ(_) -> 1099
    | OP_EQ(_) -> 1100
    | OP_GT(_) -> 1101
    | OP_LT(_) -> 1102
    | OP_MINUS(_) -> 1103
    | OP_MINUS_EQ(_) -> 1104
    | OP_MOD(_) -> 1105
    | OP_MOD_EQ(_) -> 1106
    | OP_MT(_) -> 1107
    | OP_MUL_EQ(_) -> 1108
    | OP_OR_EQ(_) -> 1109
    | OP_PLUS(_) -> 1110
    | OP_PLUS_EQ(_) -> 1111
    | OP_TILDA(_) -> 1112
    | OP_XOR_EQ(_) -> 1113
    | PLUS(_) -> 1114
    | RBRACKET(_) -> 1115
    | RPAREN(_) -> 1116
    | SEMI(_) -> 1117
    | STAR(_) -> 1118
    | STOREDPROCEDURE(_) -> 1119
    | STRING_CONST(_) -> 1120
    | WEIGHT(_) -> 1121

let private productions = [|[Ntrm 469;Trm 808];[Trm 1103];[];[Trm 963];[Ntrm 20];[Trm 1120];[Ntrm 706];[Trm 1093;Ntrm 448;Trm 1115];[Ntrm 447];[Ntrm 767];[Ntrm 37;Ntrm 797];[Ntrm 449;Ntrm 797];[];[Trm 857];[Trm 913];[Trm 982];[Trm 1049];[Trm 847];[Trm 1069];[Trm 814];[Trm 857];[Trm 913];[Trm 982];[Trm 1049];[Trm 847];[Trm 1069];[Trm 814];[Trm 809;Ntrm 37];[Ntrm 464];[Ntrm 63];[Trm 906];[];[Ntrm 450;Ntrm 464];[];[Ntrm 40];[Ntrm 19];[Ntrm 25];[Ntrm 38];[Ntrm 62];[Ntrm 6];[Ntrm 21];[Ntrm 11];[Ntrm 2];[Trm 1076;Trm 1093;Trm 947;Trm 1115];[Ntrm 70];[Ntrm 18];[Trm 1021;Ntrm 629;Ntrm 640;Ntrm 651];[Ntrm 451];[];[Ntrm 452];[];[Trm 1117];[];[Ntrm 69];[Trm 1036;Ntrm 453;Ntrm 87];[Ntrm 89];[Trm 1036;Ntrm 88];[Ntrm 751];[Ntrm 55;Ntrm 784];[];[Ntrm 90;Ntrm 784];[];[Ntrm 91];[];[Ntrm 92];[];[Ntrm 93];[];[Ntrm 94];[];[Trm 1070];[];[Ntrm 95];[];[Ntrm 762];[Ntrm 37;Ntrm 795];[];[Ntrm 96;Ntrm 795];[];[Ntrm 97];[];[Trm 871;Ntrm 98;Ntrm 798];[Ntrm 99];[];[Ntrm 101;Ntrm 798];[Ntrm 100];[Trm 1094;Ntrm 513;Ntrm 524;Ntrm 535;Ntrm 546;Ntrm 557;Ntrm 568];[Ntrm 102];[];[Ntrm 72];[];[Trm 1081];[];[Ntrm 103];[];[Ntrm 104];[];[Trm 1005];[];[Trm 858;Ntrm 105;Ntrm 580;Ntrm 37;Ntrm 591;Ntrm 707;Ntrm 602;Ntrm 613;Trm 828;Ntrm 465;Ntrm 627];[Trm 1095;Ntrm 719;Trm 1116];[Ntrm 719];[Ntrm 106];[];[Ntrm 107];[];[Ntrm 26;Ntrm 799];[Ntrm 26];[Ntrm 108];[];[Ntrm 109];[];[Trm 1117];[];[Ntrm 110;Ntrm 465];[];[Trm 1117];[];[Ntrm 53];[Ntrm 31];[Ntrm 40];[Ntrm 19];[Ntrm 85];[Ntrm 25];[Ntrm 62];[Ntrm 11];[Ntrm 2];[Ntrm 75];[Ntrm 60];[Ntrm 38];[Trm 1018;Ntrm 73];[Ntrm 21];[Ntrm 30];[Ntrm 70];[Ntrm 68];[Trm 994;Ntrm 73;Ntrm 628];[Trm 1117];[];[Trm 1085;Ntrm 73;Ntrm 54];[Trm 834;Ntrm 466;Trm 876];[Trm 1117];[];[Ntrm 111;Ntrm 466];[];[Trm 912;Ntrm 73;Ntrm 54;Ntrm 631];[Ntrm 112];[];[Trm 1000;Trm 1095;Ntrm 113;Ntrm 114;Ntrm 632;Trm 1116;Ntrm 633];[Ntrm 115];[];[Ntrm 738];[Ntrm 73;Ntrm 768];[];[Ntrm 116;Ntrm 768];[];[Ntrm 117];[];[Ntrm 739];[Ntrm 118;Ntrm 769];[];[Ntrm 119;Ntrm 769];[];[Trm 865;Ntrm 711;Ntrm 639];[Ntrm 740];[Ntrm 120;Ntrm 770];[];[Ntrm 121;Ntrm 770];[];[Trm 828];[];[Trm 937];[];[Trm 1050];[];[Trm 894];[];[Trm 828];[];[Trm 1117];[];[Trm 1056;Trm 1095;Ntrm 467;Trm 1116];[Ntrm 122;Ntrm 467];[];[Ntrm 37;Ntrm 123;Ntrm 641;Ntrm 642;Ntrm 645;Ntrm 646];[Ntrm 124];[];[Ntrm 125];[];[Ntrm 126];[];[Ntrm 127];[];[Trm 1025];[];[Ntrm 7];[];[Ntrm 128];[Ntrm 129];[];[Ntrm 130];[];[Ntrm 132];[Ntrm 131];[Ntrm 133;Ntrm 657];[Ntrm 134];[];[Ntrm 741];[Ntrm 13;Ntrm 771];[];[Ntrm 135;Ntrm 771];[];[Ntrm 136];[];[Ntrm 137];[];[Ntrm 742];[Ntrm 138;Ntrm 772];[];[Ntrm 139;Ntrm 772];[];[Ntrm 140];[];[Ntrm 141];[];[Ntrm 743];[Ntrm 142;Ntrm 773];[];[Ntrm 143;Ntrm 773];[];[Ntrm 32];[];[Ntrm 144];[];[Ntrm 744];[Ntrm 58;Ntrm 774];[];[Ntrm 145;Ntrm 774];[];[Trm 1117];[];[Ntrm 658];[Ntrm 146];[];[Trm 1089;Ntrm 147];[Ntrm 148];[];[Ntrm 149];[];[Ntrm 150];[];[Ntrm 151];[];[Ntrm 152];[];[Ntrm 153];[];[Ntrm 154];[];[Ntrm 155];[];[Ntrm 156];[];[Ntrm 157];[];[Ntrm 158];[];[Ntrm 159];[];[Ntrm 672;Ntrm 674;Ntrm 675];[Ntrm 160];[];[Ntrm 161];[];[Ntrm 162];[];[Ntrm 163];[];[Ntrm 164];[Ntrm 745];[Ntrm 165;Ntrm 775];[];[Ntrm 166;Ntrm 775];[];[Ntrm 167;Ntrm 677;Trm 828;Trm 1095;Ntrm 68;Trm 1116;Ntrm 678];[Ntrm 168];[];[Trm 1117];[];[Ntrm 169;Ntrm 679];[Trm 818];[];[Ntrm 170];[];[Ntrm 746];[Ntrm 171;Ntrm 776];[];[Ntrm 172;Ntrm 776];[];[Trm 1033;Ntrm 681;Ntrm 682;Ntrm 67;Ntrm 686;Ntrm 687;Ntrm 688;Ntrm 689;Ntrm 692];[Ntrm 173];[];[Trm 989];[];[Ntrm 174];[];[Ntrm 175];[];[Ntrm 176];[];[Ntrm 34];[];[Ntrm 177];[];[Trm 818];[];[Ntrm 178];[];[Ntrm 179];[];[Ntrm 180];[];[Ntrm 718];[Ntrm 747];[Ntrm 181;Ntrm 777];[];[Ntrm 182;Ntrm 777];[];[Ntrm 183];[];[Ntrm 184];[];[Trm 828];[];[Ntrm 185];[];[Trm 1053;Trm 1095;Ntrm 698;Ntrm 73;Trm 1116];[Trm 941;Ntrm 73];[Ntrm 186];[];[Trm 981;Trm 1095;Ntrm 720;Ntrm 48;Trm 1116];[Ntrm 748];[Ntrm 699;Ntrm 778];[];[Ntrm 187;Ntrm 778];[];[Ntrm 188];[];[Ntrm 189;Trm 1095;Ntrm 700;Trm 1116;Trm 981;Trm 1095;Ntrm 701;Ntrm 48;Trm 1116];[Ntrm 73];[];[Ntrm 190;Ntrm 468];[];[Ntrm 468];[];[Ntrm 702];[Ntrm 191];[];[Ntrm 192];[];[Ntrm 749];[Ntrm 193;Ntrm 779];[];[Ntrm 194;Ntrm 779];[];[Ntrm 195;Ntrm 456];[Trm 960];[];[Trm 960];[];[Ntrm 196;Ntrm 456];[];[Trm 814;Ntrm 474;Ntrm 475;Trm 915;Trm 1095;Ntrm 198;Trm 1116];[Trm 882;Trm 1095;Ntrm 57;Trm 1116];[Ntrm 73;Ntrm 3;Ntrm 197;Trm 1095;Ntrm 57;Trm 1116];[Ntrm 73;Trm 924;Ntrm 473;Trm 963];[Ntrm 73;Ntrm 472;Trm 835;Ntrm 73;Trm 821;Ntrm 73];[Ntrm 73;Ntrm 471];[Ntrm 199];[];[Trm 960];[];[Trm 960];[];[Ntrm 200];[];[Ntrm 201];[];[Ntrm 476];[Ntrm 202];[];[Ntrm 750];[Ntrm 79;Ntrm 780];[];[Ntrm 203;Ntrm 780];[];[Trm 1094;Trm 809;Ntrm 37;Trm 1095;Ntrm 709;Trm 1116;Ntrm 491;Ntrm 493;Ntrm 455];[Trm 1094;Ntrm 489;Ntrm 455];[Trm 1095;Ntrm 43;Trm 1116;Ntrm 455];[Ntrm 37;Ntrm 487;Ntrm 37;Ntrm 488;Ntrm 455];[Ntrm 204;Ntrm 455];[Ntrm 35;Ntrm 477;Ntrm 35;Ntrm 478;Ntrm 479;Ntrm 455];[Trm 1073;Ntrm 83;Ntrm 35;Ntrm 455];[Trm 991;Ntrm 50;Ntrm 35;Ntrm 455];[Trm 859;Trm 927;Ntrm 79;Ntrm 455];[Ntrm 42;Ntrm 79;Trm 969;Ntrm 66;Ntrm 455];[];[Trm 828];[];[Ntrm 81];[];[Trm 807;Ntrm 457];[];[Ntrm 205];[];[Trm 828];[];[Ntrm 206];[];[Ntrm 207];[];[Trm 828];[];[Ntrm 208];[];[Ntrm 209];[];[Trm 828];[];[Ntrm 210];[];[Trm 828];[];[Ntrm 211];[];[Ntrm 212];[];[Ntrm 213];[];[Ntrm 47];[Ntrm 46];[Ntrm 45];[Ntrm 33];[Ntrm 16];[Trm 972;Trm 1095;Ntrm 214;Trm 1116];[Ntrm 215];[];[Ntrm 216];[];[Ntrm 4];[];[Ntrm 497;Ntrm 498;Ntrm 499;Ntrm 500;Ntrm 501;Ntrm 502];[Ntrm 217];[];[Ntrm 218];[];[Ntrm 219];[];[Ntrm 220];[];[Ntrm 221];[];[Ntrm 222];[];[Trm 971;Trm 1095;Ntrm 35;Trm 807;Trm 1120;Trm 1116];[Trm 970;Trm 1095;Ntrm 37;Trm 807;Trm 1120;Trm 1116];[Trm 902;Trm 1095;Ntrm 35;Trm 807;Ntrm 223;Trm 807;Trm 1120;Ntrm 504;Ntrm 505;Trm 1116];[Ntrm 752];[Ntrm 35;Ntrm 781];[];[Ntrm 224;Ntrm 781];[];[Ntrm 225];[];[Ntrm 226];[];[Trm 856;Trm 1095;Ntrm 35;Trm 807;Ntrm 227;Trm 807;Trm 811;Ntrm 15;Trm 811;Ntrm 506;Ntrm 507;Trm 1116];[Ntrm 228];[];[Ntrm 229];[];[Ntrm 231];[Ntrm 230];[Ntrm 15;Ntrm 800];[Ntrm 15];[Trm 1120];[Trm 1120];[Trm 900;Trm 1095;Ntrm 232;Trm 807;Ntrm 724;Trm 1116];[Ntrm 753];[Ntrm 71;Ntrm 782];[];[Ntrm 233;Ntrm 782];[];[Ntrm 234;Ntrm 801];[Ntrm 236;Ntrm 801];[Ntrm 235];[Trm 925;Trm 1095;Ntrm 725;Trm 1116];[Ntrm 754];[Ntrm 237;Ntrm 783];[];[Ntrm 238;Ntrm 783];[];[Ntrm 239];[];[Ntrm 509;Ntrm 240];[Trm 957];[];[Trm 1057;Ntrm 510;Trm 1095;Ntrm 73;Ntrm 511;Trm 1116;Ntrm 512];[Trm 1055];[];[Ntrm 241];[];[Ntrm 242];[];[Trm 1095;Ntrm 43;Trm 1116];[Ntrm 79;Trm 859;Trm 927;Ntrm 79];[Ntrm 79;Ntrm 42;Ntrm 79;Trm 969;Ntrm 66];[Ntrm 514;Trm 927];[Trm 979];[];[Ntrm 41];[];[Ntrm 243];[];[Trm 1013];[Trm 948];[Trm 908];[Trm 940];[Trm 1095;Ntrm 802;Trm 894;Ntrm 73;Trm 915;Trm 1095;Ntrm 730;Trm 1116;Trm 1116];[Ntrm 73;Ntrm 802];[Ntrm 73];[Trm 1095;Ntrm 37;Trm 894;Ntrm 73;Trm 915;Trm 1095;Ntrm 723;Trm 1116;Trm 1116];[Trm 844;Ntrm 517;Ntrm 458;Ntrm 518;Trm 876];[Ntrm 73];[];[Ntrm 244;Ntrm 458];[];[Ntrm 245];[];[Trm 858;Trm 1056;Ntrm 519;Trm 814;Trm 1095;Ntrm 726;Ntrm 727;Trm 1116;Ntrm 522;Ntrm 523;Ntrm 525];[Ntrm 756];[Ntrm 77;Ntrm 786];[];[Ntrm 246;Ntrm 786];[];[Ntrm 755];[Ntrm 247;Ntrm 785];[];[Ntrm 248;Ntrm 785];[];[Trm 814];[];[Trm 809];[];[Ntrm 249];[];[Ntrm 250];[];[Ntrm 251];[];[Trm 1117];[];[Ntrm 530;Ntrm 533;Ntrm 459];[Ntrm 37;Ntrm 72;Ntrm 526;Ntrm 527;Ntrm 528];[Ntrm 252];[];[Ntrm 253];[];[Ntrm 254];[];[Ntrm 255];[];[Ntrm 256];[];[Ntrm 257];[];[Ntrm 258];[];[Trm 1025];[];[Ntrm 8;Ntrm 459];[];[Ntrm 534;Ntrm 259];[Ntrm 260];[];[Ntrm 261];[];[Ntrm 262];[];[Ntrm 757];[Ntrm 39;Ntrm 787];[];[Ntrm 263;Ntrm 787];[];[Ntrm 264];[];[Ntrm 265];[];[Ntrm 266];[];[Ntrm 267];[];[Ntrm 22;Ntrm 460];[];[Ntrm 268];[];[Ntrm 269];[];[Trm 814;Trm 828;Ntrm 73;Ntrm 544;Ntrm 547];[Ntrm 270];[];[Ntrm 271];[];[Ntrm 272];[];[Ntrm 273];[];[Ntrm 274];[];[Ntrm 275];[];[Ntrm 276];[];[Ntrm 277];[];[Ntrm 22;Ntrm 461];[];[Ntrm 278];[];[Ntrm 279];[];[Ntrm 280];[];[Ntrm 556;Ntrm 281];[Ntrm 758];[Ntrm 282;Ntrm 788];[];[Ntrm 283;Ntrm 788];[];[Ntrm 759];[Trm 814;Ntrm 789];[];[Ntrm 284;Ntrm 789];[];[Ntrm 285];[];[Ntrm 286];[];[Ntrm 287];[];[Ntrm 288];[];[Ntrm 289];[];[Ntrm 290];[];[Ntrm 22;Ntrm 462];[];[Ntrm 291];[];[Ntrm 292];[];[Trm 969;Ntrm 293;Ntrm 23];[Trm 1036;Trm 866];[Trm 1036;Trm 963];[Trm 843];[Trm 954;Trm 817];[Ntrm 294];[Ntrm 565;Trm 919;Ntrm 566;Ntrm 569;Ntrm 295;Ntrm 296;Ntrm 574];[Ntrm 297];[];[Trm 989];[];[Ntrm 298];[];[Trm 922];[];[Ntrm 78;Ntrm 463];[];[Ntrm 299];[];[Ntrm 300];[];[Ntrm 760];[Trm 1094;Ntrm 790];[];[Ntrm 301;Ntrm 790];[];[Ntrm 49];[];[Ntrm 302];[];[Ntrm 761];[Ntrm 303;Ntrm 791];[];[Ntrm 304;Ntrm 791];[];[Trm 1117];[];[Trm 980;Ntrm 24;Trm 922;Ntrm 305;Ntrm 575;Ntrm 576];[Ntrm 306];[];[Ntrm 307];[];[Ntrm 308;Ntrm 577;Ntrm 734];[Ntrm 763];[Ntrm 309;Ntrm 792];[];[Ntrm 310;Ntrm 792];[];[Trm 828];[];[Ntrm 311];[];[Trm 828];[];[Ntrm 312];[];[Ntrm 583;Ntrm 37];[Trm 814];[];[Ntrm 313];[];[Trm 834;Ntrm 314;Ntrm 585;Ntrm 588];[Trm 1120];[];[Ntrm 315];[];[Ntrm 316];[];[Trm 1117];[];[Trm 851;Ntrm 317;Ntrm 589;Ntrm 590];[Ntrm 318];[];[Trm 1117];[];[Ntrm 28];[Ntrm 27];[Ntrm 29];[Ntrm 592;Ntrm 319];[Trm 1095;Ntrm 735;Trm 1116];[Ntrm 735];[Ntrm 764];[Ntrm 595;Ntrm 793];[];[Ntrm 320;Ntrm 793];[];[Ntrm 765];[Ntrm 321;Ntrm 794];[];[Ntrm 322;Ntrm 794];[];[Ntrm 323];[];[Ntrm 324];[];[Ntrm 325];[];[Ntrm 326];[];[Ntrm 327];[];[Trm 866];[];[Ntrm 328];[];[Ntrm 329];[];[Ntrm 330];[];[Trm 1119;Trm 1095;Ntrm 709;Trm 1116];[Ntrm 331;Trm 1095;Ntrm 803;Trm 1116;Ntrm 603;Ntrm 604];[Trm 1114];[];[Ntrm 333;Ntrm 803];[Ntrm 332];[Ntrm 334];[];[Trm 1117];[];[Ntrm 335;Trm 1095;Ntrm 804;Ntrm 607;Trm 1116;Ntrm 609;Ntrm 610;Ntrm 611];[Trm 811];[];[Trm 1114];[];[Ntrm 337;Ntrm 804];[Ntrm 336];[Trm 980];[];[Ntrm 339;Ntrm 805];[Ntrm 338];[Ntrm 805];[];[Ntrm 340];[];[Ntrm 341];[];[Trm 1117];[];[Trm 1095;Ntrm 342;Trm 1116];[Trm 1048];[Trm 966;Ntrm 618];[Trm 951;Ntrm 617];[Trm 1079;Ntrm 616];[Trm 1080;Ntrm 615];[Trm 845;Ntrm 614];[Trm 1009];[Trm 892;Ntrm 612];[Trm 950];[Trm 1065];[Trm 920];[Trm 1054];[Trm 1046];[Trm 863];[Trm 864];[Trm 1045];[Trm 838];[Trm 964];[Trm 836];[Ntrm 82];[];[Ntrm 82];[];[Ntrm 82];[];[Ntrm 82];[];[Ntrm 82];[];[Ntrm 82];[];[Ntrm 343;Trm 828;Ntrm 344];[Ntrm 348];[Ntrm 347];[Ntrm 346];[Ntrm 345];[Trm 1107];[Trm 1102];[Trm 1112];[Trm 1105];[Trm 1098];[Trm 1118];[Trm 1103];[Trm 1110];[Trm 1100];[Trm 1109];[Trm 1113];[Trm 1097];[Trm 1106];[Trm 1099];[Trm 1108];[Trm 1104];[Trm 1111];[Trm 888;Ntrm 619;Ntrm 349;Ntrm 622];[Ntrm 350];[];[Ntrm 351];[];[Trm 905];[];[Ntrm 352];[];[Ntrm 31;Ntrm 454];[Trm 1069;Trm 903;Ntrm 35;Ntrm 454];[Trm 926;Trm 1095;Trm 1120;Trm 1116;Ntrm 454];[Trm 882;Ntrm 73;Ntrm 454];[Ntrm 2;Ntrm 454];[Ntrm 6;Ntrm 454];[Ntrm 0;Ntrm 454];[Ntrm 61;Ntrm 454];[Trm 1118;Ntrm 454];[Trm 1094;Ntrm 454];[Trm 813;Ntrm 454];[Ntrm 353;Ntrm 73;Ntrm 454];[Ntrm 68;Ntrm 454];[Ntrm 35;Ntrm 454];[Ntrm 5;Ntrm 454];[Trm 1095;Ntrm 73;Trm 1116;Ntrm 454];[Ntrm 65;Ntrm 454];[Ntrm 74;Ntrm 454];[Ntrm 626;Trm 936;Ntrm 73;Ntrm 454];[Ntrm 625;Trm 915;Trm 1095;Ntrm 709;Trm 1116;Ntrm 454];[Ntrm 354;Ntrm 73;Ntrm 454];[Trm 850;Ntrm 37;Ntrm 454];[Trm 924;Ntrm 623;Trm 963;Ntrm 454];[Ntrm 3;Ntrm 73;Ntrm 454];[];[Trm 960];[];[Trm 960];[];[Trm 960];[];[Ntrm 35;Trm 1095;Ntrm 737;Trm 1116];[Ntrm 766];[Ntrm 355;Ntrm 796];[];[Ntrm 356;Ntrm 796];[];[Ntrm 1;Ntrm 579];[Trm 1068];[Trm 1067];[Trm 1094];[Trm 814];[Trm 1087];[Trm 1051;Ntrm 357];[Trm 1040];[Trm 1039];[Trm 1038];[Trm 1014];[Trm 986];[Trm 965];[Trm 956];[Trm 955];[Trm 914];[Trm 897];[Trm 893];[Trm 999];[Trm 862];[Trm 854];[Trm 826];[Trm 825];[Trm 824];[Trm 822];[Trm 823];[Trm 968];[Trm 969];[Trm 1120;Ntrm 358;Ntrm 37;Trm 1095;Ntrm 719;Trm 1116];[Trm 1094;Ntrm 662;Trm 1100;Ntrm 73];[Ntrm 359];[Ntrm 37];[Trm 1094;Trm 1100;Trm 1094];[Trm 807;Ntrm 55];[Trm 810;Ntrm 37];[Trm 1031];[Trm 901];[Trm 887];[Trm 872];[Trm 931];[Trm 1050];[Trm 973];[Trm 1032];[Trm 1008];[Trm 967;Ntrm 730];[Trm 807;Ntrm 37];[Trm 894;Ntrm 360];[Trm 997];[Trm 996];[Ntrm 37;Trm 809];[Ntrm 503;Ntrm 37];[Ntrm 503;Ntrm 37];[Ntrm 35;Trm 809];[Trm 1100;Ntrm 361];[Trm 980];[Trm 978];[Trm 996];[Trm 997];[Ntrm 37;Trm 809];[Trm 1117;Trm 808];[Trm 1086;Ntrm 799];[Trm 894;Trm 1017];[Ntrm 54;Ntrm 624];[Ntrm 54;Ntrm 630];[Trm 875;Ntrm 54];[Trm 1094];[Trm 1120];[Ntrm 20];[Trm 807;Ntrm 20;Trm 807;Ntrm 20];[Trm 807;Ntrm 709];[Trm 807;Ntrm 73];[Trm 1086;Ntrm 710];[Trm 1037];[Trm 961];[Trm 938];[Trm 807;Ntrm 362];[Ntrm 37;Ntrm 638;Ntrm 80];[Ntrm 37;Trm 861;Ntrm 635;Ntrm 636;Ntrm 637;Ntrm 73];[Trm 1094;Ntrm 634;Ntrm 72];[Trm 807;Ntrm 363];[Ntrm 76];[Ntrm 9];[Ntrm 364];[Trm 1030];[Trm 850;Ntrm 37];[Trm 866;Ntrm 73];[Trm 808;Trm 807;Trm 808];[Trm 911;Ntrm 644];[Ntrm 643];[Trm 846;Ntrm 73];[Ntrm 648];[Ntrm 647];[Trm 960;Trm 963];[Trm 963];[Trm 1072];[Trm 993;Trm 930];[Ntrm 365;Trm 1095;Ntrm 730;Trm 1116];[Trm 846;Trm 1095;Ntrm 66;Trm 1116];[Ntrm 649;Ntrm 57;Ntrm 650;Ntrm 653;Ntrm 655;Ntrm 656];[Trm 1086;Ntrm 712];[Trm 807;Ntrm 13];[Trm 869];[Trm 829];[Trm 977;Trm 841;Ntrm 713];[Ntrm 366;Ntrm 652];[Trm 807;Ntrm 367];[Trm 841;Ntrm 709];[Trm 852;Ntrm 714;Ntrm 654];[Ntrm 368;Ntrm 73];[Trm 807;Ntrm 369];[Trm 975;Trm 1095;Ntrm 715;Trm 1116];[Trm 807;Ntrm 58];[Trm 894;Ntrm 370];[Trm 988;Ntrm 668;Ntrm 669];[Trm 884;Ntrm 666];[Ntrm 371;Ntrm 660];[Trm 1095;Trm 1120;Trm 1116];[Trm 1095;Trm 1120;Trm 1116];[Trm 807;Ntrm 372];[Trm 815];[Trm 1092];[Trm 807;Trm 874;Ntrm 665];[Ntrm 12;Ntrm 661;Ntrm 664];[Trm 807;Trm 1090];[Ntrm 12;Ntrm 667];[Trm 1095;Trm 1120;Trm 1116];[Trm 815];[Trm 1092];[Trm 807;Trm 874;Ntrm 671];[Ntrm 12;Ntrm 670];[Trm 807;Trm 837;Trm 833];[Trm 807;Trm 1069];[Trm 1095;Trm 1120;Trm 1116];[Trm 807;Trm 1023;Ntrm 676];[Trm 1076;Trm 992;Trm 1120];[Trm 946;Trm 808];[Trm 883;Trm 1082];[Trm 929;Trm 992];[Trm 928;Trm 992];[Trm 1020;Trm 992];[Trm 1010];[Trm 985;Ntrm 376];[Trm 974;Trm 894;Trm 1095;Ntrm 716;Trm 1116];[Trm 944;Trm 808];[Trm 895;Trm 977];[Trm 885;Trm 808];[Ntrm 375;Trm 927];[Ntrm 374;Trm 1071];[Ntrm 373;Trm 907];[Trm 1094;Trm 1100;Trm 1120];[Trm 807;Ntrm 377];[Ntrm 37];[Ntrm 35];[Trm 1095;Ntrm 730;Trm 1116];[Trm 1095;Ntrm 57;Trm 1116];[Ntrm 59];[Ntrm 717];[Ntrm 378;Ntrm 59];[Trm 1095;Ntrm 57;Trm 1116];[Trm 807;Ntrm 379];[Trm 870];[Trm 818];[Trm 1086;Trm 1063];[Trm 1066;Ntrm 73;Ntrm 683;Ntrm 685];[Trm 922;Ntrm 35];[Trm 1084;Ntrm 66];[Trm 1086;Ntrm 380];[Trm 907;Trm 841;Ntrm 690;Ntrm 709;Ntrm 691];[Trm 909;Ntrm 66];[Ntrm 37;Trm 1100;Ntrm 73];[Ntrm 381];[Ntrm 35;Trm 809;Trm 1118];[Trm 1118];[Trm 807;Ntrm 382];[Ntrm 37;Trm 809];[Ntrm 383;Ntrm 384];[Ntrm 697;Ntrm 37];[Trm 870];[Trm 818];[Trm 807;Ntrm 699];[Ntrm 68];[Trm 987;Trm 841;Trm 1094];[Trm 962];[Trm 1029];[Trm 868];[Trm 1001];[Ntrm 35];[Ntrm 68];[Trm 987;Trm 841;Trm 1094];[Trm 869];[Trm 829];[Trm 977;Trm 841;Ntrm 721];[Ntrm 73;Ntrm 703];[Trm 807;Ntrm 385];[Trm 1095;Ntrm 66;Trm 1116];[Ntrm 704;Ntrm 51];[Ntrm 386;Ntrm 705;Ntrm 387];[Trm 827];[Trm 1047];[Trm 818];[Ntrm 388;Trm 807;Ntrm 389];[Trm 878;Trm 1120];[Trm 809;Trm 814];[Trm 1095;Trm 1116];[Trm 903;Ntrm 722];[Trm 807;Ntrm 79];[Ntrm 73;Ntrm 484;Ntrm 486];[Ntrm 390];[Trm 1086;Trm 1095;Ntrm 78;Ntrm 457;Trm 1116];[Ntrm 482;Ntrm 37];[Trm 1095;Ntrm 730;Trm 1116];[Ntrm 485;Ntrm 37];[Trm 1095;Ntrm 730;Trm 1116];[Trm 1095;Ntrm 730;Trm 1116];[Ntrm 490;Ntrm 35];[Trm 828;Ntrm 37];[Trm 1095;Ntrm 730;Trm 1116];[Trm 840;Trm 1120;Trm 807;Ntrm 393];[Trm 1120;Trm 807;Ntrm 391;Trm 807;Ntrm 392];[Ntrm 35;Trm 809];[Ntrm 35;Trm 809];[Trm 807;Trm 849;Trm 1100;Trm 1120];[Trm 807;Trm 877;Trm 1100;Trm 1120];[Trm 807;Trm 891;Trm 1100;Trm 808];[Trm 807;Trm 934;Trm 1100;Trm 808];[Trm 807;Trm 945;Trm 1100;Trm 808];[Trm 807;Trm 1028;Trm 1100;Trm 808];[Trm 1118];[Trm 1095;Ntrm 723;Trm 1116];[Ntrm 35];[Trm 807;Ntrm 35];[Trm 807;Trm 932;Ntrm 394];[Trm 807;Trm 808];[Trm 1118];[Trm 1095;Ntrm 730;Trm 1116];[Ntrm 37];[Trm 807;Trm 932;Ntrm 395];[Trm 807;Trm 808];[Ntrm 84];[Ntrm 56];[Ntrm 36];[Ntrm 52];[Ntrm 71];[Trm 1095;Ntrm 15;Trm 1116;Ntrm 396;Ntrm 800];[Trm 1062];[Trm 917];[Trm 807;Ntrm 71];[Ntrm 52];[Ntrm 71];[Ntrm 397;Ntrm 398];[Ntrm 399;Ntrm 400];[Ntrm 401;Ntrm 508];[Trm 807;Ntrm 402];[Trm 1121;Trm 1095;Trm 808;Trm 1116];[Trm 1088];[Trm 1075];[Trm 1059];[Trm 1058];[Trm 1035];[Trm 1026];[Trm 1016];[Trm 1007];[Trm 1006];[Trm 1004];[Trm 1003];[Trm 984];[Trm 961];[Trm 958];[Trm 910];[Trm 886];[Trm 916;Trm 1095;Ntrm 730;Trm 1116];[Trm 1027];[Trm 989];[Trm 1015;Ntrm 73];[Ntrm 403;Ntrm 516];[Trm 918];[Trm 1083;Ntrm 73;Trm 1061;Ntrm 73];[Trm 875;Ntrm 73];[Trm 807;Ntrm 77];[Ntrm 14];[Ntrm 10];[Trm 807;Ntrm 404];[Trm 814;Trm 809;Ntrm 520;Ntrm 521];[Trm 969;Ntrm 405];[Trm 1060;Ntrm 406];[Trm 850;Trm 814];[Trm 960;Trm 963];[Trm 963];[Trm 855;Trm 814];[Ntrm 529;Trm 866;Ntrm 73];[Trm 1095;Ntrm 73;Trm 807;Ntrm 73;Trm 1116];[Trm 960;Trm 894;Trm 1017];[Trm 911;Ntrm 531;Ntrm 532];[Trm 846;Ntrm 543;Ntrm 73];[Ntrm 539;Trm 1011;Ntrm 540;Trm 814;Ntrm 541;Ntrm 460;Ntrm 542];[Ntrm 407;Ntrm 536;Ntrm 537;Ntrm 538];[Trm 855;Trm 814];[Trm 959];[Trm 848];[Trm 1086;Trm 1095;Ntrm 728;Trm 1116];[Trm 1086;Trm 889;Trm 1100;Trm 808];[Trm 807;Ntrm 39];[Trm 969;Ntrm 408];[Trm 898;Trm 930];[Trm 814;Trm 809];[Trm 1095;Trm 814;Trm 1116];[Trm 960;Trm 894;Trm 1017];[Trm 960;Trm 894;Trm 1017];[Trm 960;Trm 963];[Trm 990;Ntrm 545];[Trm 855;Trm 814];[Trm 959];[Trm 848];[Trm 1086;Trm 1095;Ntrm 728;Trm 1116];[Trm 1086;Trm 889;Trm 1100;Trm 808];[Trm 969;Ntrm 409];[Trm 898;Trm 930];[Trm 1095;Trm 814;Trm 1116];[Trm 960;Trm 894;Trm 1017];[Trm 960;Trm 894;Trm 1017];[Trm 846;Ntrm 555;Ntrm 73];[Ntrm 552;Trm 1011;Trm 814;Ntrm 553;Ntrm 461;Ntrm 554];[Ntrm 548;Ntrm 410;Ntrm 549;Ntrm 550;Ntrm 551];[Trm 846;Ntrm 564;Ntrm 73];[Trm 898;Trm 930;Trm 1095;Ntrm 731;Trm 1116;Trm 1011;Trm 814;Ntrm 562;Ntrm 462;Ntrm 563];[Ntrm 411;Ntrm 558;Trm 1095;Ntrm 729;Trm 1116;Ntrm 560;Ntrm 561];[Trm 814;Ntrm 559];[Trm 807;Ntrm 412];[Trm 807;Trm 814];[Trm 855;Trm 814];[Trm 959];[Trm 848];[Trm 869];[Trm 829];[Trm 1086;Trm 1095;Ntrm 728;Trm 1116];[Trm 1086;Trm 889;Trm 1100;Trm 808];[Trm 969;Ntrm 413];[Trm 1095;Ntrm 731;Trm 1116];[Trm 960;Trm 894;Trm 1017];[Trm 960;Trm 894;Trm 1017];[Trm 1074];[Trm 867];[Trm 819;Trm 1100;Ntrm 418];[Trm 820;Trm 1100;Ntrm 417];[Trm 1052;Trm 1100;Ntrm 416];[Trm 913;Trm 1100;Ntrm 415];[Trm 889;Trm 1100;Trm 808];[Trm 983;Trm 1100;Ntrm 414];[Ntrm 64;Ntrm 570];[Ntrm 44];[Ntrm 571;Ntrm 572;Ntrm 573];[Trm 1086;Ntrm 712];[Trm 1066;Ntrm 73;Ntrm 567];[Trm 1086;Trm 1095;Ntrm 463;Trm 1116];[Trm 1095;Ntrm 732;Trm 1116];[Trm 807;Trm 1094];[Trm 866;Trm 1078];[Ntrm 1];[Trm 1078;Trm 1095;Ntrm 733;Trm 1116];[Ntrm 73];[Trm 963];[Trm 866];[Trm 807;Ntrm 419];[Trm 814];[Trm 1094];[Trm 1095;Ntrm 732;Trm 1116];[Trm 980;Ntrm 24];[Ntrm 73];[Ntrm 37];[Ntrm 420;Ntrm 581];[Trm 807;Ntrm 421];[Ntrm 578;Trm 814];[Ntrm 582;Trm 814];[Trm 814;Trm 809];[Trm 814;Trm 809;Ntrm 584;Trm 809];[Trm 814;Trm 809;Trm 814;Trm 809;Trm 814;Trm 809];[Trm 1068];[Trm 1067];[Trm 1086;Trm 942;Ntrm 587];[Ntrm 422;Ntrm 586];[Trm 1068];[Trm 1067];[Trm 1094];[Trm 814];[Ntrm 17;Trm 809;Trm 814;Trm 1095;Ntrm 736;Trm 1116];[Ntrm 423];[Trm 807;Ntrm 595];[Ntrm 600;Ntrm 73];[Trm 807;Ntrm 424];[Trm 881];[Trm 880];[Trm 1094;Trm 1100];[Trm 1117;Trm 808];[Trm 1094;Trm 1100];[Trm 978];[Trm 980];[Ntrm 596;Ntrm 425];[Trm 1086;Trm 1010];[Trm 1094;Trm 1100];[Trm 881];[Trm 880];[Ntrm 426;Ntrm 601];[Ntrm 427;Ntrm 601];[Trm 828;Ntrm 428;Trm 1100;Trm 1120];[Trm 881];[Trm 880];[Trm 1120;Ntrm 605;Ntrm 606];[Trm 1094];[Trm 1120;Ntrm 605;Ntrm 606];[Trm 1094];[Trm 807;Ntrm 73;Ntrm 608];[Trm 807;Ntrm 73;Ntrm 608];[Trm 828;Ntrm 429;Trm 1100;Trm 1120];[Trm 830;Ntrm 35];[Trm 943];[Trm 808];[Trm 881];[Trm 880];[Ntrm 37;Trm 1100;Trm 1120];[Trm 982];[Trm 1034];[Trm 842];[Trm 1102;Trm 1107];[Trm 1102;Trm 1100];[Trm 1107;Trm 1100];[Trm 1102;Trm 1101];[Trm 1094];[Ntrm 430];[Trm 1012;Ntrm 432];[Trm 816;Ntrm 431];[Trm 933];[Trm 890];[Trm 995];[Trm 953];[Ntrm 620;Trm 903];[Trm 922;Ntrm 732];[Trm 1112];[Trm 1104];[Trm 1111];[Trm 960];[Trm 976];[Trm 821];[Ntrm 73];[Ntrm 72];[Trm 807;Ntrm 433];[Trm 1089];[Trm 1064];[Trm 998];[Trm 923];[Trm 810];[Trm 809];[Trm 861;Ntrm 673;Ntrm 684;Ntrm 695;Ntrm 470;Trm 894;Ntrm 68;Ntrm 481];[Trm 1074;Ntrm 492];[Trm 873;Trm 1096];[Trm 1120];[Trm 808];[Trm 963];[Trm 1037];[Trm 961];[Trm 938];[Ntrm 37;Ntrm 638;Ntrm 80];[Ntrm 37;Trm 861;Ntrm 635;Ntrm 636;Ntrm 637;Ntrm 73];[Trm 1094;Ntrm 634;Ntrm 72];[Trm 828;Ntrm 73];[Trm 1072];[Trm 993;Trm 930];[Trm 808];[Ntrm 37];[Ntrm 434;Ntrm 652];[Trm 1053];[Trm 949];[Trm 943];[Trm 857];[Trm 832];[Ntrm 435;Ntrm 73];[Ntrm 86];[Trm 839];[Trm 831];[Trm 1002;Ntrm 659];[Trm 1091;Ntrm 663];[Trm 1090];[Trm 977];[Trm 908];[Trm 948];[Trm 908];[Trm 853];[Trm 908];[Trm 948];[Trm 940];[Trm 896];[Trm 1041];[Trm 1094;Trm 1100;Trm 1120];[Trm 921];[Trm 879];[Trm 1071;Ntrm 680];[Trm 1095;Ntrm 57;Trm 1116];[Trm 1022];[Trm 860];[Ntrm 73;Ntrm 696];[Ntrm 693;Ntrm 436;Ntrm 694];[Ntrm 37;Trm 1100;Ntrm 73];[Ntrm 437];[Ntrm 35;Trm 809;Trm 1118];[Trm 1118];[Trm 810];[Trm 809];[Ntrm 37;Trm 1095;Ntrm 709;Trm 1116];[Ntrm 438];[Ntrm 73;Ntrm 703];[Trm 976];[Trm 821];[Trm 1095;Ntrm 66;Trm 1116];[Ntrm 51];[Trm 1120];[Ntrm 20];[Trm 1120];[Ntrm 20];[Ntrm 64;Ntrm 480;Ntrm 483];[Trm 1120];[Trm 1120;Trm 1117;Trm 1120;Trm 1117;Trm 1120];[Trm 1120];[Ntrm 494;Ntrm 495;Ntrm 35];[Trm 1044];[Trm 1043];[Trm 1042];[Trm 899;Trm 1100;Trm 1120;Ntrm 496];[Trm 808];[Trm 1120];[Trm 808];[Trm 1120];[Ntrm 441];[Ntrm 440];[Ntrm 439];[Trm 1112];[Trm 952];[Ntrm 52];[Ntrm 71];[Trm 1112];[Trm 952];[Ntrm 52];[Ntrm 71];[Ntrm 56];[Ntrm 36];[Ntrm 52];[Ntrm 71];[Ntrm 442;Ntrm 508];[Ntrm 443;Ntrm 515];[Ntrm 14];[Ntrm 10];[Trm 866];[Trm 814];[Trm 814;Trm 1095;Trm 814;Trm 1116];[Trm 866];[Trm 814];[Trm 1072];[Trm 993;Trm 930];[Trm 866];[Trm 814];[Trm 814;Trm 1095;Trm 814;Trm 1116];[Trm 866];[Trm 814];[Trm 814;Trm 1095;Trm 814;Trm 1116];[Trm 1072];[Trm 993;Trm 930];[Trm 1072];[Trm 993;Trm 930];[Trm 814;Ntrm 559];[Trm 866];[Trm 814];[Trm 814;Trm 1095;Trm 814;Trm 1116];[Trm 968];[Trm 969];[Trm 968];[Trm 969];[Trm 968];[Trm 969];[Trm 968];[Trm 969];[Trm 968];[Trm 969];[Ntrm 73];[Trm 963];[Trm 866];[Ntrm 73];[Ntrm 37];[Ntrm 444;Ntrm 581];[Trm 1094];[Ntrm 37];[Ntrm 593;Ntrm 445;Ntrm 708;Ntrm 599];[Ntrm 600;Ntrm 73];[Ntrm 598];[Trm 1094;Ntrm 597];[Ntrm 73];[Trm 1120];[Trm 1094];[Trm 1120];[Trm 1094];[Trm 1077];[Trm 939];[Trm 1077];[Trm 939];[Ntrm 621;Trm 814];[Trm 1094];[Ntrm 20];[Trm 1094];[Ntrm 20];[Ntrm 73];[Ntrm 72];[Trm 808];[Ntrm 37];[Trm 1053];[Trm 949];[Trm 943];[Trm 857];[Trm 832];[Trm 1024];[Trm 911];[Ntrm 37];[Ntrm 73;Ntrm 696];[Ntrm 693;Ntrm 446;Ntrm 694];[Ntrm 37];[Trm 1120];[Trm 811];[Trm 821];[Trm 811];[Trm 821;Trm 960];[Trm 811];[Trm 976];[Ntrm 56];[Ntrm 36];[Ntrm 52];[Ntrm 71];[Trm 904];[Trm 1019];[Trm 935];[Ntrm 73];[Ntrm 37];[Trm 1094];[Ntrm 35;Ntrm 594];[Trm 1024];[Trm 911];[Ntrm 37]|]
let private actions = [|(807,4),[450];(807,10),[561];(807,12),[268];(807,32),[240];(807,34),[387];(807,67),[321];(807,90),[932];(807,96),[944];(807,114),[967];(807,115),[968];(807,116),[969];(807,119),[974];(807,121),[978];(807,135),[999];(807,139),[1004];(807,143),[1008];(807,145),[1010];(807,150),[1017];(807,152),[1020];(807,153),[1021];(807,154),[1022];(807,155),[1023];(807,158),[1027];(807,159),[1028];(807,160),[1029];(807,161),[1030];(807,163),[1032];(807,166),[1049];(807,170),[1055];(807,172),[1058];(807,182),[1072];(807,187),[1078];(807,194),[1092];(807,203),[1104];(807,217),[1120];(807,218),[1121];(807,219),[1122];(807,220),[1123];(807,221),[1124];(807,222),[1125];(807,224),[1129];(807,225),[1130];(807,226),[1131];(807,228),[1135];(807,229),[1136];(807,233),[1145];(807,238),[1151];(807,246),[1177];(807,247),[1179];(807,248),[1180];(807,263),[1200];(807,283),[1226];(807,284),[1227];(807,301),[1254];(807,304),[1261];(807,310),[1269];(807,320),[1285];(807,322),[1287];(807,328),[1295];(807,338),[1309];(807,339),[1310];(807,356),[1343];(807,404),[1447];(807,425),[1489];(807,454),[886];(807,455),[405];(807,456),[370];(807,457),[410];(807,459),[580];(807,460),[602];(807,461),[625];(807,462),[656];(807,471),[378];(807,476),[389];(807,478),[409];(807,479),[413];(807,480),[417];(807,483),[419];(807,484),[423];(807,486),[425];(807,488),[429];(807,489),[433];(807,491),[435];(807,493),[437];(807,496),[448];(807,497),[451;452];(807,498),[453;454];(807,499),[455;456];(807,500),[457;458];(807,501),[459;460];(807,502),[461];(807,504),[471;472];(807,505),[473];(807,506),[476;477];(807,507),[478];(807,508),[502];(807,512),[512];(807,513),[88];(807,524),[90];(807,526),[564];(807,527),[566];(807,528),[570];(807,530),[576];(807,531),[572];(807,532),[574];(807,533),[578];(807,535),[92];(807,536),[585];(807,537),[587];(807,538),[594];(807,541),[600];(807,542),[604];(807,544),[611];(807,545),[609];(807,546),[94];(807,547),[631];(807,549),[615];(807,550),[617];(807,551),[619];(807,553),[623];(807,554),[627];(807,557),[96];(807,559),[648];(807,560),[650];(807,561),[652];(807,562),[654];(807,563),[658];(807,568),[98];(807,581),[716];(807,585),[728];(807,586),[726];(807,587),[724];(807,588),[730];(807,594),[757];(807,595),[764;765];(807,596),[759];(807,597),[761];(807,598),[763];(807,605),[782];(807,606),[784];(807,607),[791];(807,608),[788];(807,612),[820];(807,614),[822];(807,615),[824];(807,616),[826];(807,617),[828];(807,618),[830];(807,622),[861];(807,632),[148];(807,650),[214];(807,652),[212];(807,653),[223];(807,654),[221];(807,655),[229;230];(807,656),[232];(807,657),[239];(807,658),[242];(807,659),[245];(807,660),[254;255];(807,661),[248;249];(807,663),[247];(807,664),[252;253];(807,665),[251];(807,666),[258;259];(807,667),[256;257];(807,668),[261];(807,669),[266;267];(807,670),[264;265];(807,671),[263];(807,672),[269;270];(807,674),[271;272];(807,675),[275;276];(807,676),[274];(807,678),[287];(807,679),[291;292];(807,681),[300];(807,682),[306];(807,683),[302];(807,685),[304];(807,686),[308];(807,687),[309;310];(807,688),[312];(807,689),[318];(807,690),[314];(807,691),[316];(807,692),[320];(807,694),[330];(807,696),[334];(807,699),[346];(807,703),[356];(807,708),[741];(807,709),[150];(807,713),[215];(807,714),[224];(807,717),[293];(807,718),[322];(807,720),[340];(807,722),[390];(807,726),[544];(807,732),[682];(807,735),[742];(807,738),[152];(807,742),[217];(807,743),[226];(807,746),[295];(807,747),[324];(807,748),[341];(807,750),[392];(807,755),[545];(807,760),[684];(807,764),[743];(807,768),[153;154];(807,769),[160];(807,770),[166];(807,771),[209];(807,772),[218;219];(807,773),[227;228];(807,774),[236];(807,775),[281];(807,776),[296;297];(807,777),[325;326];(807,778),[343];(807,779),[362];(807,780),[393;394];(807,781),[469];(807,782),[490];(807,783),[499];(807,784),[60];(807,785),[547];(807,786),[542];(807,787),[591];(807,788),[636];(807,789),[641];(807,790),[685;686];(807,791),[694];(807,792),[707];(807,793),[745];(807,794),[750];(807,795),[77];(807,796),[897];(807,797),[12];(807,805),[789;790];(808,12),[268];(808,20),[0];(808,24),[703];(808,32),[240];(808,34),[387];(808,43),[514;515];(808,51),[373;374;375;376];(808,66),[364];(808,67),[321];(808,73),[879];(808,74),[4];(808,79),[399];(808,113),[966];(808,126),[985];(808,138),[1003];(808,153),[1021];(808,155),[1023];(808,159),[1028];(808,170),[1055];(808,181),[1069];(808,193),[1091];(808,195),[1094];(808,198),[1099];(808,204),[1105];(808,303),[1258];(808,308),[1266];(808,309),[1268];(808,321),[1286];(808,328),[1295];(808,342),[1314];(808,355),[1341];(808,361),[1354];(808,366),[1365];(808,367),[1367];(808,381),[1397];(808,382),[1400];(808,385),[1407];(808,387),[1411];(808,388),[1413];(808,389),[1415];(808,394),[1425];(808,395),[1427];(808,419),[1479];(808,420),[1482];(808,421),[1484];(808,424),[1488];(808,425),[1491];(808,431),[1502];(808,432),[1504];(808,433),[1505];(808,434),[1507];(808,437),[1517];(808,444),[1534];(808,454),[886];(808,455),[405];(808,456),[370];(808,469),[2];(808,471),[378];(808,476),[389];(808,478),[409];(808,479),[413];(808,480),[417];(808,483),[419];(808,484),[423];(808,486),[425];(808,488),[429];(808,489),[433];(808,491),[435];(808,493),[437];(808,512),[512];(808,517),[532];(808,543),[606];(808,555),[629];(808,564),[660];(808,577),[712];(808,585),[728];(808,586),[726];(808,587),[724];(808,588),[730];(808,594),[757];(808,595),[764];(808,596),[759];(808,600),[769];(808,622),[861];(808,635),[171];(808,636),[173];(808,637),[175];(808,644),[188];(808,650),[214];(808,652),[212];(808,653),[223];(808,654),[221];(808,655),[229;230];(808,656),[232];(808,657),[239];(808,658),[242];(808,659),[245];(808,660),[254;255];(808,661),[249];(808,663),[247];(808,664),[253];(808,665),[251];(808,666),[258;259];(808,667),[257];(808,668),[261];(808,669),[266;267];(808,670),[265];(808,671),[263];(808,672),[270];(808,674),[272];(808,675),[276];(808,676),[274];(808,679),[291;292];(808,681),[300];(808,682),[306];(808,683),[302];(808,685),[304];(808,686),[308];(808,687),[309;310];(808,688),[312];(808,689),[318];(808,690),[314];(808,691),[316];(808,692),[320];(808,694),[330];(808,696),[334];(808,698),[338];(808,700),[348];(808,704),[366];(808,705),[368];(808,708),[741];(808,709),[150];(808,713),[215];(808,714),[224];(808,717),[293];(808,718),[322];(808,721),[359];(808,722),[390];(808,732),[682];(808,733),[691];(808,734),[704];(808,735),[742];(808,736),[747];(808,737),[894];(808,738),[151;152];(808,742),[216;217];(808,743),[226];(808,746),[295];(808,747),[323;324];(808,749),[360];(808,750),[391;392];(808,760),[684];(808,761),[692];(808,763),[705];(808,764),[743];(808,765),[748];(808,766),[895];(808,768),[154];(808,772),[219];(808,773),[228];(808,776),[297];(808,777),[326];(808,780),[394];(808,790),[686];(808,797),[12];(808,802),[528;529];(809,184),[1074];(809,200),[1101];(809,358),[1349];(809,383),[1404];(809,449),[27];(809,474),[383];(809,520),[550];(809,521),[551];(809,584),[719];(809,694),[329];(809,797),[11;12];(810,91),[933];(810,184),[1074];(810,358),[1348];(810,383),[1403];(810,662),[62];(810,694),[329];(811,396),[1429;1430;1431];(811,439),[1521];(811,440),[1523];(811,441),[1525];(811,605),[781];(812,12),[268];(812,32),[240];(812,34),[387];(812,63),[28];(812,67),[321];(812,153),[1021];(812,155),[1023];(812,159),[1028];(812,170),[1055];(812,296),[1249];(812,328),[1295];(812,425),[1489];(812,454),[886];(812,455),[405];(812,456),[370];(812,464),[33];(812,465),[115];(812,471),[378];(812,476),[389];(812,478),[409];(812,479),[413];(812,480),[417];(812,481),[80];(812,483),[419];(812,484),[423];(812,486),[425];(812,488),[429];(812,489),[433];(812,491),[435];(812,492),[73];(812,493),[437];(812,512),[512];(812,522),[556];(812,523),[558];(812,525),[560];(812,570),[679];(812,571),[681];(812,572),[688];(812,573),[690];(812,574),[697];(812,575),[700];(812,576),[702];(812,577),[712];(812,579),[31];(812,581),[716];(812,585),[728];(812,586),[726];(812,587),[724];(812,588),[730];(812,589),[733];(812,590),[735];(812,594),[757];(812,595),[764;765];(812,596),[759];(812,597),[761];(812,598),[763];(812,599),[767];(812,603),[777];(812,604),[779];(812,609),[794];(812,610),[796];(812,611),[798];(812,612),[820];(812,614),[822];(812,615),[824];(812,616),[826];(812,617),[828];(812,618),[830];(812,622),[861];(812,624),[113];(812,627),[117];(812,628),[137];(812,629),[48];(812,631),[146];(812,633),[156];(812,639),[179];(812,640),[50];(812,650),[214];(812,651),[52];(812,652),[212];(812,653),[223];(812,654),[221];(812,655),[229;230];(812,656),[232];(812,657),[239];(812,658),[242];(812,659),[245];(812,660),[254;255];(812,661),[249];(812,663),[247];(812,664),[253];(812,665),[251];(812,666),[258;259];(812,667),[257];(812,668),[261];(812,669),[266;267];(812,670),[265];(812,671),[263];(812,672),[270];(812,674),[272];(812,675),[276];(812,676),[274];(812,679),[291;292];(812,681),[300];(812,682),[306];(812,683),[302];(812,685),[304];(812,686),[308];(812,687),[309;310];(812,688),[312];(812,689),[318];(812,690),[314];(812,691),[316];(812,692),[320];(812,694),[330];(812,696),[334];(812,708),[741];(812,709),[150];(812,710),[157];(812,711),[163];(812,713),[215];(812,714),[224];(812,717),[293];(812,718),[322];(812,722),[390];(812,730),[74];(812,732),[682];(812,734),[704];(812,735),[742];(812,738),[152];(812,739),[159];(812,740),[165];(812,742),[217];(812,743),[226];(812,746),[295];(812,747),[324];(812,750),[392];(812,760),[684];(812,762),[76];(812,763),[706];(812,764),[743;744];(812,768),[154];(812,769),[161];(812,770),[167];(812,772),[219];(812,773),[228];(812,776),[297];(812,777),[326];(812,780),[394];(812,790),[686];(812,792),[708];(812,793),[746];(812,795),[78];(812,797),[12];(812,806),[29];(813,12),[268];(813,24),[703];(813,32),[240];(813,34),[387];(813,43),[514;515];(813,51),[373;374;375;376];(813,66),[364];(813,67),[321];(813,73),[872];(813,79),[399];(813,153),[1021];(813,155),[1023];(813,159),[1028];(813,170),[1055];(813,181),[1069];(813,193),[1091];(813,195),[1094];(813,204),[1105];(813,303),[1258];(813,308),[1266];(813,309),[1268];(813,321),[1286];(813,328),[1295];(813,355),[1341];(813,381),[1397];(813,382),[1400];(813,385),[1407];(813,387),[1411];(813,419),[1479];(813,420),[1482];(813,421),[1484];(813,424),[1488];(813,425),[1491];(813,433),[1505];(813,437),[1517];(813,444),[1534];(813,454),[886];(813,455),[405];(813,456),[370];(813,471),[378];(813,476),[389];(813,478),[409];(813,479),[413];(813,480),[417];(813,483),[419];(813,484),[423];(813,486),[425];(813,488),[429];(813,489),[433];(813,491),[435];(813,493),[437];(813,512),[512];(813,517),[532];(813,543),[606];(813,555),[629];(813,564),[660];(813,577),[712];(813,585),[728];(813,586),[726];(813,587),[724];(813,588),[730];(813,594),[757];(813,595),[764];(813,596),[759];(813,600),[769];(813,622),[861];(813,635),[171];(813,636),[173];(813,637),[175];(813,650),[214];(813,652),[212];(813,653),[223];(813,654),[221];(813,655),[229;230];(813,656),[232];(813,657),[239];(813,658),[242];(813,659),[245];(813,660),[254;255];(813,661),[249];(813,663),[247];(813,664),[253];(813,665),[251];(813,666),[258;259];(813,667),[257];(813,668),[261];(813,669),[266;267];(813,670),[265];(813,671),[263];(813,672),[270];(813,674),[272];(813,675),[276];(813,676),[274];(813,679),[291;292];(813,681),[300];(813,682),[306];(813,683),[302];(813,685),[304];(813,686),[308];(813,687),[309;310];(813,688),[312];(813,689),[318];(813,690),[314];(813,691),[316];(813,692),[320];(813,694),[330];(813,696),[334];(813,698),[338];(813,700),[348];(813,704),[366];(813,705),[368];(813,708),[741];(813,709),[150];(813,713),[215];(813,714),[224];(813,717),[293];(813,718),[322];(813,721),[359];(813,722),[390];(813,732),[682];(813,733),[691];(813,734),[704];(813,735),[742];(813,736),[747];(813,737),[894];(813,738),[151;152];(813,742),[217];(813,743),[226];(813,746),[295];(813,747),[323;324];(813,749),[360];(813,750),[391;392];(813,760),[684];(813,761),[692];(813,763),[705];(813,764),[743];(813,765),[748];(813,766),[895];(813,768),[154];(813,772),[219];(813,773),[228];(813,776),[297];(813,777),[326];(813,780),[394];(813,790),[686];(813,797),[12];(813,802),[528;529];(814,1),[44];(814,5),[893];(814,7),[196];(814,9),[183];(814,10),[562];(814,12),[268];(814,13),[283];(814,14),[607];(814,24),[703];(814,29),[739];(814,30),[738];(814,32),[240];(814,34),[387];(814,35),[9];(814,37),[6];(814,43),[514;515];(814,44),[717];(814,51),[371;373;374;375;376];(814,54),[132;133];(814,63),[28];(814,66),[364];(814,67),[321];(814,69),[55];(814,70),[53];(814,73),[875;876];(814,79),[398;399;400];(814,89),[930];(814,99),[948];(814,100),[949];(814,101),[950];(814,102),[951];(814,106),[957];(814,110),[961];(814,111),[962];(814,120),[975;976];(814,122),[980];(814,127),[987];(814,128),[989;990];(814,138),[1003];(814,153),[1021];(814,155),[1023];(814,159),[1028];(814,167),[1050;1051];(814,170),[1055];(814,181),[1068;1069;1070];(814,183),[1073];(814,185),[1075];(814,190),[1085];(814,193),[1091];(814,195),[1094];(814,204),[1105];(814,206),[1108];(814,208),[1110];(814,211),[1113];(814,215),[1118];(814,216),[1119];(814,223),[1128];(814,227),[1134];(814,247),[1178;1179];(814,249),[1181];(814,266),[1203];(814,282),[1225];(814,295),[1248];(814,296),[1249];(814,302),[1256];(814,303),[1258];(814,305),[1262];(814,308),[1266;1267];(814,309),[1268];(814,311),[1270];(814,312),[1271];(814,313),[1272;1273;1274];(814,316),[1278];(814,318),[1282];(814,319),[1284];(814,321),[1286];(814,328),[1295];(814,344),[1317];(814,349),[1326];(814,355),[1341];(814,363),[1359;1360];(814,366),[1366];(814,367),[1367];(814,381),[1397;1398];(814,382),[1399;1400;1401];(814,384),[1405;1406];(814,385),[1407];(814,387),[1411];(814,392),[1420];(814,404),[1446;1447];(814,405),[1449;1450];(814,406),[1452];(814,408),[1456;1457];(814,409),[1459;1460];(814,412),[1465];(814,413),[1467;1468];(814,419),[1479];(814,420),[1482;1483];(814,421),[1484];(814,422),[1486];(814,423),[1487];(814,424),[1488];(814,425),[1489;1491];(814,430),[1500];(814,433),[1505];(814,434),[1508];(814,436),[1516];(814,437),[1517;1518];(814,438),[1519];(814,444),[1534;1535];(814,445),[1537];(814,446),[1540];(814,447),[19];(814,448),[26];(814,450),[899];(814,452),[903];(814,454),[886];(814,455),[405];(814,456),[370];(814,464),[32];(814,465),[114;115];(814,466),[142];(814,467),[181];(814,468),[350];(814,471),[378];(814,476),[389];(814,477),[407];(814,478),[409];(814,479),[413];(814,480),[416;417];(814,481),[80];(814,482),[415];(814,483),[419];(814,484),[422;423];(814,485),[421];(814,486),[425];(814,487),[427];(814,488),[429];(814,489),[432;433];(814,490),[431];(814,491),[435];(814,492),[73];(814,493),[437];(814,494),[444;445];(814,495),[446;447];(814,503),[82;83];(814,512),[512];(814,513),[87];(814,517),[532];(814,519),[553;554];(814,520),[549;550];(814,521),[552];(814,522),[556];(814,523),[558];(814,525),[560];(814,540),[597;598];(814,543),[606];(814,555),[629];(814,564),[660];(814,566),[673];(814,567),[671];(814,569),[675];(814,570),[679];(814,571),[681];(814,572),[688];(814,573),[689;690];(814,574),[697];(814,575),[700];(814,576),[702];(814,577),[711;712];(814,578),[710];(814,579),[31];(814,580),[102;103];(814,581),[715;716];(814,582),[714];(814,583),[720;721];(814,584),[718];(814,585),[727;728];(814,586),[726];(814,587),[724];(814,588),[730];(814,589),[732;733];(814,590),[735];(814,592),[753];(814,593),[755];(814,594),[757];(814,595),[764;765];(814,596),[759];(814,597),[761];(814,598),[763];(814,599),[767];(814,600),[769];(814,603),[777];(814,604),[779];(814,609),[794];(814,610),[796];(814,611),[798];(814,612),[820];(814,614),[822];(814,615),[824];(814,616),[826];(814,617),[828];(814,618),[830];(814,619),[857];(814,621),[859];(814,622),[861];(814,624),[113];(814,627),[117];(814,628),[137];(814,629),[48];(814,630),[141];(814,631),[146];(814,633),[156];(814,635),[171];(814,636),[173];(814,637),[175];(814,639),[179];(814,640),[49;50];(814,641),[185];(814,642),[190;191];(814,643),[187];(814,644),[189];(814,645),[193];(814,646),[194;195];(814,647),[198];(814,648),[200];(814,650),[214];(814,651),[52];(814,652),[212];(814,653),[223];(814,654),[221];(814,655),[229;230];(814,656),[232];(814,657),[239];(814,658),[242];(814,659),[245];(814,660),[254;255];(814,661),[249];(814,663),[247];(814,664),[253];(814,665),[251];(814,666),[258;259];(814,667),[257];(814,668),[261];(814,669),[266;267];(814,670),[265];(814,671),[263];(814,672),[270];(814,674),[272];(814,675),[276];(814,676),[274];(814,679),[291;292];(814,681),[300];(814,682),[306];(814,683),[302];(814,685),[304];(814,686),[308];(814,687),[309;310];(814,688),[312];(814,689),[318];(814,690),[314];(814,691),[316];(814,692),[320];(814,693),[327;328];(814,694),[330];(814,696),[333;334];(814,697),[332];(814,698),[338];(814,700),[348];(814,701),[352];(814,704),[366];(814,705),[368];(814,706),[8];(814,708),[741];(814,709),[150];(814,710),[157];(814,711),[163];(814,712),[206];(814,713),[215];(814,714),[224];(814,717),[293];(814,718),[322];(814,721),[359];(814,722),[390];(814,723),[466];(814,726),[544];(814,729),[633];(814,730),[74];(814,731),[638];(814,732),[682];(814,733),[691];(814,734),[704];(814,735),[742];(814,736),[747];(814,737),[894];(814,738),[151;152];(814,739),[159];(814,740),[164;165];(814,741),[207];(814,742),[216;217];(814,743),[226];(814,746),[295];(814,747),[323;324];(814,749),[360];(814,750),[391;392];(814,752),[467];(814,755),[545];(814,758),[634];(814,759),[639];(814,760),[684];(814,761),[692];(814,762),[75;76];(814,763),[705;706];(814,764),[743;744];(814,765),[748];(814,766),[895];(814,767),[10];(814,768),[154];(814,769),[161];(814,770),[167];(814,772),[219];(814,773),[228];(814,776),[297];(814,777),[326];(814,780),[394];(814,790),[686];(814,792),[708];(814,793),[746];(814,795),[78];(814,797),[12];(814,798),[84;85];(814,802),[528;529];(814,806),[29];(815,151),[1018];(815,157),[1025];(815,665),[250];(815,671),[262];(816,350),[1328];(816,351),[1333];(816,619),[856];(816,620),[854];(818,173),[1060];(818,186),[1077];(818,197),[1098];(818,680),[289];(818,681),[299];(818,690),[313];(818,698),[337];(819,39),[666];(819,294),[1241];(819,728),[588];(819,757),[589];(820,39),[666];(820,294),[1242];(820,728),[588];(820,757),[589];(821,12),[268];(821,32),[240];(821,34),[387];(821,67),[321];(821,153),[1021];(821,155),[1023];(821,159),[1028];(821,170),[1055];(821,196),[1095];(821,354),[1340];(821,386),[1409];(821,396),[1430;1431];(821,439),[1522];(821,440),[1524];(821,454),[882;886];(821,455),[405];(821,456),[369;370];(821,471),[378];(821,476),[389];(821,478),[409];(821,479),[413];(821,480),[417];(821,483),[419];(821,484),[423];(821,486),[425];(821,488),[429];(821,489),[433];(821,491),[435];(821,493),[437];(821,512),[512];(821,585),[728];(821,586),[726];(821,587),[724];(821,588),[730];(821,622),[861];(821,650),[214];(821,652),[212];(821,653),[223];(821,654),[221];(821,655),[229;230];(821,656),[232];(821,657),[239];(821,658),[242];(821,659),[245];(821,660),[254;255];(821,661),[249];(821,663),[247];(821,664),[253];(821,665),[251];(821,666),[258;259];(821,667),[257];(821,668),[261];(821,669),[266;267];(821,670),[265];(821,671),[263];(821,672),[270];(821,674),[272];(821,675),[276];(821,676),[274];(821,679),[291;292];(821,681),[300];(821,682),[306];(821,683),[302];(821,685),[304];(821,686),[308];(821,687),[309;310];(821,688),[312];(821,689),[318];(821,690),[314];(821,691),[316];(821,692),[320];(821,694),[330];(821,696),[334];(821,709),[150];(821,713),[215];(821,714),[224];(821,717),[293];(821,718),[322];(821,722),[390];(821,732),[682];(821,738),[152];(821,742),[217];(821,743),[226];(821,746),[295];(821,747),[324];(821,750),[392];(821,760),[684];(821,768),[154];(821,772),[219];(821,773),[228];(821,776),[297];(821,777),[326];(821,780),[394];(821,790),[686];(821,797),[12];(822,453),[923];(823,453),[924];(824,453),[922];(825,453),[921];(826,453),[920];(827,197),[1096];(828,12),[268];(828,32),[240];(828,34),[387];(828,67),[321];(828,123),[981];(828,153),[1021];(828,155),[1023];(828,159),[1028];(828,170),[1055];(828,185),[1075];(828,206),[1108];(828,208),[1110];(828,211),[1113];(828,212),[1114];(828,311),[1270];(828,312),[1271];(828,334),[1302];(828,340),[1311];(828,364),[1362];(828,454),[886];(828,455),[405];(828,456),[370];(828,471),[378];(828,476),[389];(828,477),[406];(828,478),[409];(828,479),[413];(828,480),[416;417];(828,482),[414];(828,483),[419];(828,484),[422;423];(828,485),[420];(828,486),[425];(828,487),[426];(828,488),[429];(828,489),[432;433];(828,490),[430];(828,491),[434;435];(828,493),[437];(828,512),[512];(828,513),[88];(828,524),[90];(828,535),[92];(828,546),[94];(828,557),[96];(828,568),[98];(828,577),[711];(828,578),[709];(828,581),[715];(828,582),[713];(828,585),[728];(828,586),[726];(828,587),[724];(828,588),[730];(828,591),[105];(828,602),[109];(828,603),[776];(828,609),[793];(828,612),[820];(828,613),[111];(828,614),[822];(828,615),[824];(828,616),[826];(828,617),[828];(828,618),[830];(828,622),[861];(828,634),[168];(828,638),[176];(828,650),[214];(828,652),[212];(828,653),[223];(828,654),[221];(828,655),[229;230];(828,656),[232];(828,657),[239];(828,658),[242];(828,659),[245];(828,660),[254;255];(828,661),[249];(828,663),[247];(828,664),[253];(828,665),[251];(828,666),[258;259];(828,667),[257];(828,668),[261];(828,669),[266;267];(828,670),[265];(828,671),[263];(828,672),[270];(828,674),[272];(828,675),[276];(828,676),[274];(828,677),[285];(828,679),[291;292];(828,681),[300];(828,682),[306];(828,683),[302];(828,685),[304];(828,686),[308];(828,687),[309;310];(828,688),[312];(828,689),[318];(828,690),[314];(828,691),[316];(828,692),[320];(828,694),[330];(828,696),[333;334];(828,697),[331];(828,707),[101];(828,709),[150];(828,713),[215];(828,714),[224];(828,717),[293];(828,718),[322];(828,719),[57];(828,722),[390];(828,732),[682];(828,738),[152];(828,742),[217];(828,743),[226];(828,746),[295];(828,747),[324];(828,750),[392];(828,751),[59];(828,760),[684];(828,768),[154];(828,772),[219];(828,773),[228];(828,776),[297];(828,777),[326];(828,780),[394];(828,784),[61];(828,790),[686];(828,797),[12];(829,12),[268];(829,32),[240];(829,34),[387];(829,67),[321];(829,136),[1001];(829,153),[1021];(829,155),[1023];(829,159),[1028];(829,170),[1055];(829,191),[1089];(829,287),[1232];(829,454),[886];(829,455),[405];(829,456),[370];(829,471),[378];(829,476),[389];(829,478),[409];(829,479),[413];(829,480),[417];(829,483),[419];(829,484),[423];(829,486),[425];(829,488),[429];(829,489),[433];(829,491),[435];(829,493),[437];(829,512),[512];(829,559),[647];(829,585),[728];(829,586),[726];(829,587),[724];(829,588),[730];(829,622),[861];(829,650),[214];(829,652),[211;212];(829,653),[223];(829,654),[221];(829,655),[229;230];(829,656),[232];(829,657),[239];(829,658),[242];(829,659),[245];(829,660),[254;255];(829,661),[249];(829,663),[247];(829,664),[253];(829,665),[251];(829,666),[258;259];(829,667),[257];(829,668),[261];(829,669),[266;267];(829,670),[265];(829,671),[263];(829,672),[270];(829,674),[272];(829,675),[276];(829,676),[274];(829,679),[291;292];(829,681),[300];(829,682),[306];(829,683),[302];(829,685),[304];(829,686),[308];(829,687),[309;310];(829,688),[312];(829,689),[318];(829,690),[314];(829,691),[316];(829,692),[320];(829,694),[330];(829,696),[334];(829,703),[355];(829,709),[150];(829,713),[215];(829,714),[224];(829,717),[293];(829,718),[322];(829,722),[390];(829,732),[682];(829,738),[152];(829,742),[217];(829,743),[226];(829,746),[295];(829,747),[324];(829,750),[392];(829,760),[684];(829,768),[154];(829,772),[219];(829,773),[228];(829,776),[297];(829,777),[326];(829,780),[394];(829,790),[686];(829,797),[12];(830,341),[1312];(830,609),[794];(830,610),[795];(831,147),[1014];(831,371),[1376];(832,142),[1007];(832,368),[1372];(832,369),[1373];(832,435),[1513];(832,714),[224];(832,743),[225];(834,1),[42];(834,2),[722];(834,12),[268];(834,24),[703];(834,32),[240];(834,34),[387];(834,43),[514;515];(834,51),[373;374;375;376];(834,54),[126;127];(834,63),[28];(834,66),[364];(834,67),[321];(834,73),[866];(834,75),[139];(834,79),[399];(834,110),[961];(834,111),[962];(834,153),[1021];(834,155),[1023];(834,159),[1028];(834,170),[1055];(834,181),[1069];(834,193),[1091];(834,195),[1094];(834,204),[1105];(834,296),[1249];(834,302),[1256];(834,303),[1258];(834,308),[1266];(834,309),[1268];(834,321),[1286];(834,328),[1295];(834,355),[1341];(834,381),[1397];(834,382),[1400];(834,385),[1407];(834,387),[1411];(834,419),[1479];(834,420),[1482];(834,421),[1484];(834,424),[1488];(834,425),[1489;1491];(834,433),[1505];(834,437),[1517];(834,444),[1534];(834,450),[899];(834,454),[886];(834,455),[405];(834,456),[370];(834,464),[32];(834,465),[114;115];(834,466),[142];(834,471),[378];(834,476),[389];(834,478),[409];(834,479),[413];(834,480),[417];(834,481),[80];(834,483),[419];(834,484),[423];(834,486),[425];(834,488),[429];(834,489),[433];(834,491),[435];(834,492),[73];(834,493),[437];(834,512),[512];(834,517),[532];(834,522),[556];(834,523),[558];(834,525),[560];(834,543),[606];(834,555),[629];(834,564),[660];(834,570),[679];(834,571),[681];(834,572),[688];(834,573),[689;690];(834,574),[697];(834,575),[700];(834,576),[702];(834,577),[712];(834,579),[31];(834,581),[716];(834,585),[728];(834,586),[726];(834,587),[724];(834,588),[730];(834,589),[733];(834,590),[735];(834,594),[757];(834,595),[764;765];(834,596),[759];(834,597),[761];(834,598),[763];(834,599),[767];(834,600),[769];(834,603),[777];(834,604),[779];(834,609),[794];(834,610),[796];(834,611),[798];(834,612),[820];(834,614),[822];(834,615),[824];(834,616),[826];(834,617),[828];(834,618),[830];(834,622),[861];(834,624),[113];(834,627),[117];(834,628),[137];(834,629),[48];(834,630),[141];(834,631),[146];(834,633),[156];(834,635),[171];(834,636),[173];(834,637),[175];(834,639),[179];(834,640),[50];(834,650),[214];(834,651),[52];(834,652),[212];(834,653),[223];(834,654),[221];(834,655),[229;230];(834,656),[232];(834,657),[239];(834,658),[242];(834,659),[245];(834,660),[254;255];(834,661),[249];(834,663),[247];(834,664),[253];(834,665),[251];(834,666),[258;259];(834,667),[257];(834,668),[261];(834,669),[266;267];(834,670),[265];(834,671),[263];(834,672),[270];(834,674),[272];(834,675),[276];(834,676),[274];(834,679),[291;292];(834,681),[300];(834,682),[306];(834,683),[302];(834,685),[304];(834,686),[308];(834,687),[309;310];(834,688),[312];(834,689),[318];(834,690),[314];(834,691),[316];(834,692),[320];(834,694),[330];(834,696),[334];(834,698),[338];(834,700),[348];(834,704),[366];(834,705),[368];(834,708),[741];(834,709),[150];(834,710),[157];(834,711),[163];(834,713),[215];(834,714),[224];(834,717),[293];(834,718),[322];(834,721),[359];(834,722),[390];(834,730),[74];(834,732),[682];(834,733),[691];(834,734),[704];(834,735),[742];(834,736),[747];(834,737),[894];(834,738),[151;152];(834,739),[159];(834,740),[165];(834,742),[217];(834,743),[226];(834,746),[295];(834,747),[323;324];(834,749),[360];(834,750),[391;392];(834,760),[684];(834,761),[692];(834,762),[76];(834,763),[705;706];(834,764),[743;744];(834,765),[748];(834,766),[895];(834,768),[154];(834,769),[161];(834,770),[167];(834,772),[219];(834,773),[228];(834,776),[297];(834,777),[326];(834,780),[394];(834,790),[686];(834,792),[708];(834,793),[746];(834,795),[78];(834,797),[12];(834,802),[528;529];(834,806),[29];(835,12),[268];(835,32),[240];(835,34),[387];(835,67),[321];(835,153),[1021];(835,155),[1023];(835,159),[1028];(835,170),[1055];(835,454),[886];(835,455),[405];(835,456),[370];(835,471),[378];(835,472),[380];(835,476),[389];(835,478),[409];(835,479),[413];(835,480),[417];(835,483),[419];(835,484),[423];(835,486),[425];(835,488),[429];(835,489),[433];(835,491),[435];(835,493),[437];(835,512),[512];(835,585),[728];(835,586),[726];(835,587),[724];(835,588),[730];(835,622),[861];(835,650),[214];(835,652),[212];(835,653),[223];(835,654),[221];(835,655),[229;230];(835,656),[232];(835,657),[239];(835,658),[242];(835,659),[245];(835,660),[254;255];(835,661),[249];(835,663),[247];(835,664),[253];(835,665),[251];(835,666),[258;259];(835,667),[257];(835,668),[261];(835,669),[266;267];(835,670),[265];(835,671),[263];(835,672),[270];(835,674),[272];(835,675),[276];(835,676),[274];(835,679),[291;292];(835,681),[300];(835,682),[306];(835,683),[302];(835,685),[304];(835,686),[308];(835,687),[309;310];(835,688),[312];(835,689),[318];(835,690),[314];(835,691),[316];(835,692),[320];(835,694),[330];(835,696),[334];(835,709),[150];(835,713),[215];(835,714),[224];(835,717),[293];(835,718),[322];(835,722),[390];(835,732),[682];(835,738),[152];(835,742),[217];(835,743),[226];(835,746),[295];(835,747),[324];(835,750),[392];(835,760),[684];(835,768),[154];(835,772),[219];(835,773),[228];(835,776),[297];(835,777),[326];(835,780),[394];(835,790),[686];(835,797),[12];(836,72),[818];(836,355),[1342];(836,433),[1506];(836,513),[88];(836,524),[89];(836,634),[169];(836,737),[894];(836,766),[895];(838,72),[816];(838,355),[1342];(838,433),[1506];(838,513),[88];(838,524),[89];(838,634),[169];(838,737),[894];(838,766),[895];(839,370),[1375];(840,214),[1116];(841,12),[268];(841,32),[240];(841,34),[387];(841,67),[321];(841,140),[1005];(841,153),[1021];(841,155),[1023];(841,159),[1028];(841,170),[1055];(841,454),[886];(841,455),[405];(841,456),[370];(841,471),[378];(841,476),[389];(841,478),[409];(841,479),[413];(841,480),[417];(841,483),[419];(841,484),[423];(841,486),[425];(841,488),[429];(841,489),[433];(841,491),[435];(841,493),[437];(841,512),[512];(841,585),[728];(841,586),[726];(841,587),[724];(841,588),[730];(841,622),[861];(841,650),[214];(841,652),[212];(841,653),[223];(841,654),[220;221];(841,655),[229;230];(841,656),[232];(841,657),[239];(841,658),[242];(841,659),[245];(841,660),[254;255];(841,661),[249];(841,663),[247];(841,664),[253];(841,665),[251];(841,666),[258;259];(841,667),[257];(841,668),[261];(841,669),[266;267];(841,670),[265];(841,671),[263];(841,672),[270];(841,674),[272];(841,675),[276];(841,676),[274];(841,679),[291;292];(841,681),[300];(841,682),[306];(841,683),[302];(841,685),[304];(841,686),[308];(841,687),[309;310];(841,688),[312];(841,689),[318];(841,690),[314];(841,691),[316];(841,692),[320];(841,694),[330];(841,696),[334];(841,709),[150];(841,713),[215];(841,714),[224];(841,717),[293];(841,718),[322];(841,722),[390];(841,732),[682];(841,738),[152];(841,742),[217];(841,743),[226];(841,746),[295];(841,747),[324];(841,750),[392];(841,760),[684];(841,768),[154];(841,772),[219];(841,773),[228];(841,776),[297];(841,777),[326];(841,780),[394];(841,790),[686];(841,797),[12];(842,344),[1320];(843,23),[664];(844,1),[39];(844,6),[531];(844,12),[268];(844,24),[703];(844,32),[240];(844,34),[387];(844,43),[514;515];(844,51),[373;374;375;376];(844,63),[28];(844,66),[364];(844,67),[321];(844,73),[867];(844,79),[399];(844,153),[1021];(844,155),[1023];(844,159),[1028];(844,170),[1055];(844,181),[1069];(844,193),[1091];(844,195),[1094];(844,204),[1105];(844,296),[1249];(844,302),[1256];(844,303),[1258];(844,308),[1266];(844,309),[1268];(844,321),[1286];(844,328),[1295];(844,355),[1341];(844,381),[1397];(844,382),[1400];(844,385),[1407];(844,387),[1411];(844,419),[1479];(844,420),[1482];(844,421),[1484];(844,424),[1488];(844,425),[1489;1491];(844,433),[1505];(844,437),[1517];(844,444),[1534];(844,450),[899];(844,454),[886];(844,455),[405];(844,456),[370];(844,464),[32];(844,465),[115];(844,471),[378];(844,476),[389];(844,478),[409];(844,479),[413];(844,480),[417];(844,481),[80];(844,483),[419];(844,484),[423];(844,486),[425];(844,488),[429];(844,489),[433];(844,491),[435];(844,492),[73];(844,493),[437];(844,512),[512];(844,517),[532];(844,522),[556];(844,523),[558];(844,525),[560];(844,543),[606];(844,555),[629];(844,564),[660];(844,570),[679];(844,571),[681];(844,572),[688];(844,573),[689;690];(844,574),[697];(844,575),[700];(844,576),[702];(844,577),[712];(844,579),[31];(844,581),[716];(844,585),[728];(844,586),[726];(844,587),[724];(844,588),[730];(844,589),[733];(844,590),[735];(844,594),[757];(844,595),[764;765];(844,596),[759];(844,597),[761];(844,598),[763];(844,599),[767];(844,600),[769];(844,603),[777];(844,604),[779];(844,609),[794];(844,610),[796];(844,611),[798];(844,612),[820];(844,614),[822];(844,615),[824];(844,616),[826];(844,617),[828];(844,618),[830];(844,622),[861];(844,624),[113];(844,627),[117];(844,628),[137];(844,629),[48];(844,631),[146];(844,633),[156];(844,635),[171];(844,636),[173];(844,637),[175];(844,639),[179];(844,640),[50];(844,650),[214];(844,651),[52];(844,652),[212];(844,653),[223];(844,654),[221];(844,655),[229;230];(844,656),[232];(844,657),[239];(844,658),[242];(844,659),[245];(844,660),[254;255];(844,661),[249];(844,663),[247];(844,664),[253];(844,665),[251];(844,666),[258;259];(844,667),[257];(844,668),[261];(844,669),[266;267];(844,670),[265];(844,671),[263];(844,672),[270];(844,674),[272];(844,675),[276];(844,676),[274];(844,679),[291;292];(844,681),[300];(844,682),[306];(844,683),[302];(844,685),[304];(844,686),[308];(844,687),[309;310];(844,688),[312];(844,689),[318];(844,690),[314];(844,691),[316];(844,692),[320];(844,694),[330];(844,696),[334];(844,698),[338];(844,700),[348];(844,704),[366];(844,705),[368];(844,708),[741];(844,709),[150];(844,710),[157];(844,711),[163];(844,713),[215];(844,714),[224];(844,717),[293];(844,718),[322];(844,721),[359];(844,722),[390];(844,730),[74];(844,732),[682];(844,733),[691];(844,734),[704];(844,735),[742];(844,736),[747];(844,737),[894];(844,738),[151;152];(844,739),[159];(844,740),[165];(844,742),[217];(844,743),[226];(844,746),[295];(844,747),[323;324];(844,749),[360];(844,750),[391;392];(844,760),[684];(844,761),[692];(844,762),[76];(844,763),[705;706];(844,764),[743;744];(844,765),[748];(844,766),[895];(844,768),[154];(844,769),[161];(844,770),[167];(844,772),[219];(844,773),[228];(844,776),[297];(844,777),[326];(844,780),[394];(844,790),[686];(844,792),[708];(844,793),[746];(844,795),[78];(844,797),[12];(844,802),[528;529];(844,806),[29];(845,72),[805];(845,355),[1342];(845,433),[1506];(845,513),[88];(845,524),[89];(845,634),[169];(845,737),[894];(845,766),[895];(846,7),[196];(846,8),[581];(846,10),[561];(846,12),[268];(846,32),[240];(846,34),[387];(846,67),[321];(846,76),[201];(846,77),[632];(846,122),[979];(846,127),[987];(846,128),[988;989;990];(846,132),[996];(846,153),[1021];(846,155),[1023];(846,159),[1028];(846,170),[1055];(846,247),[1179];(846,259),[1192];(846,280),[1219];(846,281),[1222];(846,404),[1447];(846,454),[886];(846,455),[405];(846,456),[370];(846,459),[579;580];(846,460),[602];(846,461),[625];(846,467),[181];(846,471),[378];(846,476),[389];(846,478),[409];(846,479),[413];(846,480),[417];(846,483),[419];(846,484),[423];(846,486),[425];(846,488),[429];(846,489),[433];(846,491),[435];(846,493),[437];(846,512),[512];(846,526),[564];(846,527),[566];(846,528),[570];(846,530),[576];(846,531),[572];(846,532),[574];(846,533),[578];(846,534),[583];(846,536),[585];(846,537),[587];(846,538),[594];(846,541),[600];(846,542),[604];(846,544),[611];(846,545),[609];(846,547),[630;631];(846,549),[615];(846,550),[617];(846,551),[619];(846,553),[623];(846,554),[627];(846,556),[644];(846,585),[728];(846,586),[726];(846,587),[724];(846,588),[730];(846,612),[820];(846,614),[822];(846,615),[824];(846,616),[826];(846,617),[828];(846,618),[830];(846,622),[861];(846,641),[185];(846,642),[190;191];(846,643),[187];(846,644),[189];(846,645),[193];(846,646),[194;195];(846,647),[198];(846,648),[200];(846,650),[214];(846,652),[212];(846,653),[223];(846,654),[221];(846,655),[229;230];(846,656),[232];(846,657),[239];(846,658),[242];(846,659),[245];(846,660),[254;255];(846,661),[249];(846,663),[247];(846,664),[253];(846,665),[251];(846,666),[258;259];(846,667),[257];(846,668),[261];(846,669),[266;267];(846,670),[265];(846,671),[263];(846,672),[270];(846,674),[272];(846,675),[276];(846,676),[274];(846,679),[291;292];(846,681),[300];(846,682),[306];(846,683),[302];(846,685),[304];(846,686),[308];(846,687),[309;310];(846,688),[312];(846,689),[318];(846,690),[314];(846,691),[316];(846,692),[320];(846,694),[330];(846,696),[334];(846,709),[150];(846,713),[215];(846,714),[224];(846,717),[293];(846,718),[322];(846,722),[390];(846,726),[544];(846,727),[539];(846,732),[682];(846,738),[152];(846,742),[217];(846,743),[226];(846,746),[295];(846,747),[324];(846,750),[392];(846,755),[545;546];(846,756),[540];(846,760),[684];(846,768),[154];(846,772),[219];(846,773),[228];(846,776),[297];(846,777),[326];(846,780),[394];(846,785),[548];(846,790),[686];(846,797),[12];(847,1),[44];(847,5),[893];(847,7),[196];(847,9),[183];(847,10),[562];(847,12),[268];(847,13),[283];(847,24),[703];(847,29),[739];(847,30),[738];(847,32),[240];(847,34),[387];(847,35),[9];(847,37),[6];(847,43),[514;515];(847,44),[717];(847,51),[373;374;375;376];(847,54),[132;133];(847,63),[28];(847,66),[364];(847,67),[321];(847,69),[55];(847,70),[53];(847,73),[875;876];(847,79),[398;399;400];(847,89),[930];(847,99),[948];(847,100),[949];(847,101),[950];(847,102),[951];(847,106),[957];(847,110),[961];(847,111),[962];(847,120),[975;976];(847,122),[980];(847,127),[987];(847,128),[989;990];(847,138),[1003];(847,153),[1021];(847,155),[1023];(847,159),[1028];(847,167),[1050;1051];(847,170),[1055];(847,181),[1068;1069;1070];(847,183),[1073];(847,185),[1075];(847,190),[1085];(847,193),[1091];(847,195),[1094];(847,204),[1105];(847,206),[1108];(847,208),[1110];(847,211),[1113];(847,215),[1118];(847,216),[1119];(847,223),[1128];(847,227),[1134];(847,247),[1179];(847,295),[1248];(847,296),[1249];(847,302),[1256];(847,303),[1258];(847,308),[1266;1267];(847,309),[1268];(847,316),[1278];(847,319),[1284];(847,321),[1286];(847,328),[1295];(847,344),[1317];(847,355),[1341];(847,363),[1359;1360];(847,366),[1366];(847,367),[1367];(847,381),[1397;1398];(847,382),[1399;1400;1401];(847,384),[1405;1406];(847,385),[1407];(847,387),[1411];(847,392),[1420];(847,404),[1447];(847,419),[1479];(847,420),[1482;1483];(847,421),[1484];(847,422),[1486];(847,423),[1487];(847,424),[1488];(847,425),[1489;1491];(847,433),[1505];(847,434),[1508];(847,436),[1516];(847,437),[1517;1518];(847,438),[1519];(847,444),[1534;1535];(847,445),[1537];(847,446),[1540];(847,447),[17];(847,448),[24];(847,450),[899];(847,454),[886];(847,455),[405];(847,456),[370];(847,464),[32];(847,465),[114;115];(847,466),[142];(847,467),[181];(847,468),[350];(847,471),[378];(847,476),[389];(847,477),[407];(847,478),[409];(847,479),[413];(847,480),[416;417];(847,481),[80];(847,482),[415];(847,483),[419];(847,484),[422;423];(847,485),[421];(847,486),[425];(847,487),[427];(847,488),[429];(847,489),[432;433];(847,490),[431];(847,491),[435];(847,492),[73];(847,493),[437];(847,494),[444;445];(847,495),[446;447];(847,503),[82;83];(847,512),[512];(847,513),[87];(847,517),[532];(847,522),[556];(847,523),[558];(847,525),[560];(847,543),[606];(847,555),[629];(847,564),[660];(847,566),[673];(847,567),[671];(847,569),[675];(847,570),[679];(847,571),[681];(847,572),[688];(847,573),[689;690];(847,574),[697];(847,575),[700];(847,576),[702];(847,577),[712];(847,579),[31];(847,580),[102;103];(847,581),[716];(847,583),[721];(847,585),[727;728];(847,586),[726];(847,587),[724];(847,588),[730];(847,589),[733];(847,590),[735];(847,592),[753];(847,593),[755];(847,594),[757];(847,595),[764;765];(847,596),[759];(847,597),[761];(847,598),[763];(847,599),[767];(847,600),[769];(847,603),[777];(847,604),[779];(847,609),[794];(847,610),[796];(847,611),[798];(847,612),[820];(847,614),[822];(847,615),[824];(847,616),[826];(847,617),[828];(847,618),[830];(847,622),[861];(847,624),[113];(847,627),[117];(847,628),[137];(847,629),[48];(847,630),[141];(847,631),[146];(847,633),[156];(847,635),[171];(847,636),[173];(847,637),[175];(847,639),[179];(847,640),[50];(847,641),[185];(847,642),[190;191];(847,643),[187];(847,644),[189];(847,645),[193];(847,646),[194;195];(847,647),[198];(847,648),[200];(847,650),[214];(847,651),[52];(847,652),[212];(847,653),[223];(847,654),[221];(847,655),[229;230];(847,656),[232];(847,657),[239];(847,658),[242];(847,659),[245];(847,660),[254;255];(847,661),[249];(847,663),[247];(847,664),[253];(847,665),[251];(847,666),[258;259];(847,667),[257];(847,668),[261];(847,669),[266;267];(847,670),[265];(847,671),[263];(847,672),[270];(847,674),[272];(847,675),[276];(847,676),[274];(847,679),[291;292];(847,681),[300];(847,682),[306];(847,683),[302];(847,685),[304];(847,686),[308];(847,687),[309;310];(847,688),[312];(847,689),[318];(847,690),[314];(847,691),[316];(847,692),[320];(847,693),[327;328];(847,694),[330];(847,696),[333;334];(847,697),[332];(847,698),[338];(847,700),[348];(847,701),[352];(847,704),[366];(847,705),[368];(847,706),[8];(847,708),[741];(847,709),[150];(847,710),[157];(847,711),[163];(847,712),[206];(847,713),[215];(847,714),[224];(847,717),[293];(847,718),[322];(847,721),[359];(847,722),[390];(847,723),[466];(847,726),[544];(847,730),[74];(847,732),[682];(847,733),[691];(847,734),[704];(847,735),[742];(847,736),[747];(847,737),[894];(847,738),[151;152];(847,739),[159];(847,740),[164;165];(847,741),[207];(847,742),[216;217];(847,743),[226];(847,746),[295];(847,747),[323;324];(847,749),[360];(847,750),[391;392];(847,752),[467];(847,755),[545];(847,760),[684];(847,761),[692];(847,762),[75;76];(847,763),[705;706];(847,764),[743;744];(847,765),[748];(847,766),[895];(847,767),[10];(847,768),[154];(847,769),[161];(847,770),[167];(847,772),[219];(847,773),[228];(847,776),[297];(847,777),[326];(847,780),[394];(847,790),[686];(847,792),[708];(847,793),[746];(847,795),[78];(847,797),[12];(847,798),[84;85];(847,802),[528;529];(847,806),[29];(848,261),[1197];(848,273),[1211];(848,286),[1230];(848,536),[584];(848,549),[614];(848,558),[645];(850,12),[268];(850,32),[240];(850,34),[387];(850,67),[321];(850,124),[983];(850,153),[1021];(850,155),[1023];(850,159),[1028];(850,170),[1055];(850,252),[1184];(850,454),[883;886];(850,455),[405];(850,456),[370];(850,471),[378];(850,476),[389];(850,478),[409];(850,479),[413];(850,480),[417];(850,483),[419];(850,484),[423];(850,486),[425];(850,488),[429];(850,489),[433];(850,491),[435];(850,493),[437];(850,512),[512];(850,526),[563];(850,585),[728];(850,586),[726];(850,587),[724];(850,588),[730];(850,612),[820];(850,614),[822];(850,615),[824];(850,616),[826];(850,617),[828];(850,618),[830];(850,622),[861];(850,641),[184];(850,650),[214];(850,652),[212];(850,653),[223];(850,654),[221];(850,655),[229;230];(850,656),[232];(850,657),[239];(850,658),[242];(850,659),[245];(850,660),[254;255];(850,661),[249];(850,663),[247];(850,664),[253];(850,665),[251];(850,666),[258;259];(850,667),[257];(850,668),[261];(850,669),[266;267];(850,670),[265];(850,671),[263];(850,672),[270];(850,674),[272];(850,675),[276];(850,676),[274];(850,679),[291;292];(850,681),[300];(850,682),[306];(850,683),[302];(850,685),[304];(850,686),[308];(850,687),[309;310];(850,688),[312];(850,689),[318];(850,690),[314];(850,691),[316];(850,692),[320];(850,694),[330];(850,696),[334];(850,709),[150];(850,713),[215];(850,714),[224];(850,717),[293];(850,718),[322];(850,722),[390];(850,732),[682];(850,738),[152];(850,742),[217];(850,743),[226];(850,746),[295];(850,747),[324];(850,750),[392];(850,760),[684];(850,768),[154];(850,772),[219];(850,773),[228];(850,776),[297];(850,777),[326];(850,780),[394];(850,790),[686];(850,797),[12];(851,1),[41];(851,11),[731];(851,12),[268];(851,32),[240];(851,34),[387];(851,54),[125];(851,63),[28];(851,67),[321];(851,110),[961];(851,111),[962];(851,153),[1021];(851,155),[1023];(851,159),[1028];(851,170),[1055];(851,296),[1249];(851,302),[1256];(851,328),[1295];(851,425),[1489];(851,450),[899];(851,454),[886];(851,455),[405];(851,456),[370];(851,464),[32];(851,465),[114;115];(851,466),[142];(851,471),[378];(851,476),[389];(851,478),[409];(851,479),[413];(851,480),[417];(851,481),[80];(851,483),[419];(851,484),[423];(851,486),[425];(851,488),[429];(851,489),[433];(851,491),[435];(851,492),[73];(851,493),[437];(851,512),[512];(851,522),[556];(851,523),[558];(851,525),[560];(851,570),[679];(851,571),[681];(851,572),[688];(851,573),[689;690];(851,574),[697];(851,575),[700];(851,576),[702];(851,577),[712];(851,579),[31];(851,581),[716];(851,585),[728];(851,586),[726];(851,587),[724];(851,588),[730];(851,589),[733];(851,590),[735];(851,594),[757];(851,595),[764;765];(851,596),[759];(851,597),[761];(851,598),[763];(851,599),[767];(851,603),[777];(851,604),[779];(851,609),[794];(851,610),[796];(851,611),[798];(851,612),[820];(851,614),[822];(851,615),[824];(851,616),[826];(851,617),[828];(851,618),[830];(851,622),[861];(851,624),[113];(851,627),[117];(851,628),[137];(851,629),[48];(851,630),[141];(851,631),[146];(851,633),[156];(851,639),[179];(851,640),[50];(851,650),[214];(851,651),[52];(851,652),[212];(851,653),[223];(851,654),[221];(851,655),[229;230];(851,656),[232];(851,657),[239];(851,658),[242];(851,659),[245];(851,660),[254;255];(851,661),[249];(851,663),[247];(851,664),[253];(851,665),[251];(851,666),[258;259];(851,667),[257];(851,668),[261];(851,669),[266;267];(851,670),[265];(851,671),[263];(851,672),[270];(851,674),[272];(851,675),[276];(851,676),[274];(851,679),[291;292];(851,681),[300];(851,682),[306];(851,683),[302];(851,685),[304];(851,686),[308];(851,687),[309;310];(851,688),[312];(851,689),[318];(851,690),[314];(851,691),[316];(851,692),[320];(851,694),[330];(851,696),[334];(851,708),[741];(851,709),[150];(851,710),[157];(851,711),[163];(851,713),[215];(851,714),[224];(851,717),[293];(851,718),[322];(851,722),[390];(851,730),[74];(851,732),[682];(851,734),[704];(851,735),[742];(851,738),[152];(851,739),[159];(851,740),[165];(851,742),[217];(851,743),[226];(851,746),[295];(851,747),[324];(851,750),[392];(851,760),[684];(851,762),[76];(851,763),[706];(851,764),[743;744];(851,768),[154];(851,769),[161];(851,770),[167];(851,772),[219];(851,773),[228];(851,776),[297];(851,777),[326];(851,780),[394];(851,790),[686];(851,792),[708];(851,793),[746];(851,795),[78];(851,797),[12];(851,806),[29];(852,12),[268];(852,32),[240];(852,34),[387];(852,67),[321];(852,141),[1006];(852,153),[1021];(852,155),[1023];(852,159),[1028];(852,170),[1055];(852,454),[886];(852,455),[405];(852,456),[370];(852,471),[378];(852,476),[389];(852,478),[409];(852,479),[413];(852,480),[417];(852,483),[419];(852,484),[423];(852,486),[425];(852,488),[429];(852,489),[433];(852,491),[435];(852,493),[437];(852,512),[512];(852,585),[728];(852,586),[726];(852,587),[724];(852,588),[730];(852,622),[861];(852,650),[214];(852,652),[212];(852,653),[222;223];(852,654),[221];(852,655),[229;230];(852,656),[232];(852,657),[239];(852,658),[242];(852,659),[245];(852,660),[254;255];(852,661),[249];(852,663),[247];(852,664),[253];(852,665),[251];(852,666),[258;259];(852,667),[257];(852,668),[261];(852,669),[266;267];(852,670),[265];(852,671),[263];(852,672),[270];(852,674),[272];(852,675),[276];(852,676),[274];(852,679),[291;292];(852,681),[300];(852,682),[306];(852,683),[302];(852,685),[304];(852,686),[308];(852,687),[309;310];(852,688),[312];(852,689),[318];(852,690),[314];(852,691),[316];(852,692),[320];(852,694),[330];(852,696),[334];(852,709),[150];(852,713),[215];(852,714),[224];(852,717),[293];(852,718),[322];(852,722),[390];(852,732),[682];(852,738),[152];(852,742),[217];(852,743),[226];(852,746),[295];(852,747),[324];(852,750),[392];(852,760),[684];(852,768),[154];(852,772),[219];(852,773),[228];(852,776),[297];(852,777),[326];(852,780),[394];(852,790),[686];(852,797),[12];(853,58),[277];(853,164),[1046];(853,374),[1384];(853,715),[233];(853,744),[234];(854,453),[919];(855,8),[581];(855,10),[561];(855,12),[268];(855,32),[240];(855,34),[387];(855,67),[321];(855,77),[632];(855,153),[1021];(855,155),[1023];(855,159),[1028];(855,170),[1055];(855,247),[1179];(855,254),[1187];(855,255),[1188];(855,260),[1195];(855,272),[1209];(855,280),[1221];(855,285),[1228];(855,404),[1447];(855,454),[886];(855,455),[405];(855,456),[370];(855,459),[579;580];(855,460),[602];(855,461),[625];(855,471),[378];(855,476),[389];(855,478),[409];(855,479),[413];(855,480),[417];(855,483),[419];(855,484),[423];(855,486),[425];(855,488),[429];(855,489),[433];(855,491),[435];(855,493),[437];(855,512),[512];(855,526),[564];(855,527),[566];(855,528),[569;570];(855,529),[567];(855,530),[576];(855,531),[572];(855,532),[574];(855,533),[578];(855,534),[582];(855,536),[585];(855,537),[587];(855,538),[594];(855,541),[600];(855,542),[604];(855,544),[611];(855,545),[609];(855,547),[630;631];(855,548),[612];(855,549),[615];(855,550),[617];(855,551),[619];(855,553),[623];(855,554),[627];(855,556),[643];(855,585),[728];(855,586),[726];(855,587),[724];(855,588),[730];(855,612),[820];(855,614),[822];(855,615),[824];(855,616),[826];(855,617),[828];(855,618),[830];(855,622),[861];(855,650),[214];(855,652),[212];(855,653),[223];(855,654),[221];(855,655),[229;230];(855,656),[232];(855,657),[239];(855,658),[242];(855,659),[245];(855,660),[254;255];(855,661),[249];(855,663),[247];(855,664),[253];(855,665),[251];(855,666),[258;259];(855,667),[257];(855,668),[261];(855,669),[266;267];(855,670),[265];(855,671),[263];(855,672),[270];(855,674),[272];(855,675),[276];(855,676),[274];(855,679),[291;292];(855,681),[300];(855,682),[306];(855,683),[302];(855,685),[304];(855,686),[308];(855,687),[309;310];(855,688),[312];(855,689),[318];(855,690),[314];(855,691),[316];(855,692),[320];(855,694),[330];(855,696),[334];(855,709),[150];(855,713),[215];(855,714),[224];(855,717),[293];(855,718),[322];(855,722),[390];(855,726),[544];(855,727),[539];(855,732),[682];(855,738),[152];(855,742),[217];(855,743),[226];(855,746),[295];(855,747),[324];(855,750),[392];(855,755),[545;546];(855,756),[540];(855,760),[684];(855,768),[154];(855,772),[219];(855,773),[228];(855,776),[297];(855,777),[326];(855,780),[394];(855,785),[548];(855,790),[686];(855,797),[12];(856,12),[268];(856,16),[475];(856,32),[240];(856,34),[387];(856,43),[514;515];(856,64),[442];(856,67),[321];(856,79),[399];(856,153),[1021];(856,155),[1023];(856,159),[1028];(856,170),[1055];(856,204),[1106];(856,295),[1247];(856,390),[1416];(856,454),[886];(856,455),[405];(856,456),[370];(856,471),[378];(856,476),[389];(856,478),[409];(856,479),[413];(856,480),[417];(856,483),[419];(856,484),[423];(856,486),[425];(856,488),[429];(856,489),[433];(856,491),[435];(856,493),[437];(856,512),[512];(856,566),[673];(856,567),[671];(856,569),[675];(856,585),[728];(856,586),[726];(856,587),[724];(856,588),[730];(856,622),[861];(856,650),[214];(856,652),[212];(856,653),[223];(856,654),[221];(856,655),[229;230];(856,656),[232];(856,657),[239];(856,658),[242];(856,659),[245];(856,660),[254;255];(856,661),[249];(856,663),[247];(856,664),[253];(856,665),[251];(856,666),[258;259];(856,667),[257];(856,668),[261];(856,669),[266;267];(856,670),[265];(856,671),[263];(856,672),[270];(856,674),[272];(856,675),[276];(856,676),[274];(856,679),[291;292];(856,681),[300];(856,682),[306];(856,683),[302];(856,685),[304];(856,686),[308];(856,687),[309;310];(856,688),[312];(856,689),[318];(856,690),[314];(856,691),[316];(856,692),[320];(856,694),[330];(856,696),[334];(856,709),[150];(856,713),[215];(856,714),[224];(856,717),[293];(856,718),[322];(856,722),[390];(856,732),[682];(856,738),[152];(856,742),[217];(856,743),[226];(856,746),[295];(856,747),[324];(856,750),[391;392];(856,760),[684];(856,768),[154];(856,772),[219];(856,773),[228];(856,776),[297];(856,777),[326];(856,780),[394];(856,790),[686];(856,797),[12];(857,1),[44];(857,5),[893];(857,7),[196];(857,9),[183];(857,10),[562];(857,12),[268];(857,13),[283];(857,24),[703];(857,29),[739];(857,30),[738];(857,32),[240];(857,34),[387];(857,35),[9];(857,37),[6];(857,43),[514;515];(857,44),[717];(857,51),[373;374;375;376];(857,54),[132;133];(857,63),[28];(857,66),[364];(857,67),[321];(857,69),[55];(857,70),[53];(857,73),[875;876];(857,79),[398;399;400];(857,89),[930];(857,99),[948];(857,100),[949];(857,101),[950];(857,102),[951];(857,106),[957];(857,110),[961];(857,111),[962];(857,120),[975;976];(857,122),[980];(857,127),[987];(857,128),[989;990];(857,138),[1003];(857,142),[1007];(857,153),[1021];(857,155),[1023];(857,159),[1028];(857,167),[1050;1051];(857,170),[1055];(857,181),[1068;1069;1070];(857,183),[1073];(857,185),[1075];(857,190),[1085];(857,193),[1091];(857,195),[1094];(857,204),[1105];(857,206),[1108];(857,208),[1110];(857,211),[1113];(857,215),[1118];(857,216),[1119];(857,223),[1128];(857,227),[1134];(857,247),[1179];(857,295),[1248];(857,296),[1249];(857,302),[1256];(857,303),[1258];(857,308),[1266;1267];(857,309),[1268];(857,316),[1278];(857,319),[1284];(857,321),[1286];(857,328),[1295];(857,344),[1317];(857,355),[1341];(857,363),[1359;1360];(857,366),[1366];(857,367),[1367];(857,368),[1371];(857,369),[1373];(857,381),[1397;1398];(857,382),[1399;1400;1401];(857,384),[1405;1406];(857,385),[1407];(857,387),[1411];(857,392),[1420];(857,404),[1447];(857,419),[1479];(857,420),[1482;1483];(857,421),[1484];(857,422),[1486];(857,423),[1487];(857,424),[1488];(857,425),[1489;1491];(857,433),[1505];(857,434),[1508];(857,435),[1512];(857,436),[1516];(857,437),[1517;1518];(857,438),[1519];(857,444),[1534;1535];(857,445),[1537];(857,446),[1540];(857,447),[13];(857,448),[20];(857,450),[899];(857,454),[886];(857,455),[405];(857,456),[370];(857,464),[32];(857,465),[114;115];(857,466),[142];(857,467),[181];(857,468),[350];(857,471),[378];(857,476),[389];(857,477),[407];(857,478),[409];(857,479),[413];(857,480),[416;417];(857,481),[80];(857,482),[415];(857,483),[419];(857,484),[422;423];(857,485),[421];(857,486),[425];(857,487),[427];(857,488),[429];(857,489),[432;433];(857,490),[431];(857,491),[435];(857,492),[73];(857,493),[437];(857,494),[444;445];(857,495),[446;447];(857,503),[82;83];(857,512),[512];(857,513),[87];(857,517),[532];(857,522),[556];(857,523),[558];(857,525),[560];(857,543),[606];(857,555),[629];(857,564),[660];(857,566),[673];(857,567),[671];(857,569),[675];(857,570),[679];(857,571),[681];(857,572),[688];(857,573),[689;690];(857,574),[697];(857,575),[700];(857,576),[702];(857,577),[712];(857,579),[31];(857,580),[102;103];(857,581),[716];(857,583),[721];(857,585),[727;728];(857,586),[726];(857,587),[724];(857,588),[730];(857,589),[733];(857,590),[735];(857,592),[753];(857,593),[755];(857,594),[757];(857,595),[764;765];(857,596),[759];(857,597),[761];(857,598),[763];(857,599),[767];(857,600),[769];(857,603),[777];(857,604),[779];(857,609),[794];(857,610),[796];(857,611),[798];(857,612),[820];(857,614),[822];(857,615),[824];(857,616),[826];(857,617),[828];(857,618),[830];(857,622),[861];(857,624),[113];(857,627),[117];(857,628),[137];(857,629),[48];(857,630),[141];(857,631),[146];(857,633),[156];(857,635),[171];(857,636),[173];(857,637),[175];(857,639),[179];(857,640),[50];(857,641),[185];(857,642),[190;191];(857,643),[187];(857,644),[189];(857,645),[193];(857,646),[194;195];(857,647),[198];(857,648),[200];(857,650),[214];(857,651),[52];(857,652),[212];(857,653),[223];(857,654),[221];(857,655),[229;230];(857,656),[232];(857,657),[239];(857,658),[242];(857,659),[245];(857,660),[254;255];(857,661),[249];(857,663),[247];(857,664),[253];(857,665),[251];(857,666),[258;259];(857,667),[257];(857,668),[261];(857,669),[266;267];(857,670),[265];(857,671),[263];(857,672),[270];(857,674),[272];(857,675),[276];(857,676),[274];(857,679),[291;292];(857,681),[300];(857,682),[306];(857,683),[302];(857,685),[304];(857,686),[308];(857,687),[309;310];(857,688),[312];(857,689),[318];(857,690),[314];(857,691),[316];(857,692),[320];(857,693),[327;328];(857,694),[330];(857,696),[333;334];(857,697),[332];(857,698),[338];(857,700),[348];(857,701),[352];(857,704),[366];(857,705),[368];(857,706),[8];(857,708),[741];(857,709),[150];(857,710),[157];(857,711),[163];(857,712),[206];(857,713),[215];(857,714),[224];(857,717),[293];(857,718),[322];(857,721),[359];(857,722),[390];(857,723),[466];(857,726),[544];(857,730),[74];(857,732),[682];(857,733),[691];(857,734),[704];(857,735),[742];(857,736),[747];(857,737),[894];(857,738),[151;152];(857,739),[159];(857,740),[164;165];(857,741),[207];(857,742),[216;217];(857,743),[225;226];(857,746),[295];(857,747),[323;324];(857,749),[360];(857,750),[391;392];(857,752),[467];(857,755),[545];(857,760),[684];(857,761),[692];(857,762),[75;76];(857,763),[705;706];(857,764),[743;744];(857,765),[748];(857,766),[895];(857,767),[10];(857,768),[154];(857,769),[161];(857,770),[167];(857,772),[219];(857,773),[228];(857,776),[297];(857,777),[326];(857,780),[394];(857,790),[686];(857,792),[708];(857,793),[746];(857,795),[78];(857,797),[12];(857,798),[84;85];(857,802),[528;529];(857,806),[29];(858,1),[35;45];(858,12),[268];(858,18),[99];(858,19),[538];(858,32),[240];(858,34),[387];(858,54),[121];(858,63),[28];(858,67),[321];(858,110),[961];(858,111),[962];(858,153),[1021];(858,155),[1023];(858,159),[1028];(858,170),[1055];(858,296),[1249];(858,302),[1256];(858,328),[1295];(858,425),[1489];(858,450),[899];(858,454),[886];(858,455),[405];(858,456),[370];(858,464),[32];(858,465),[114;115];(858,466),[142];(858,471),[378];(858,476),[389];(858,478),[409];(858,479),[413];(858,480),[417];(858,481),[80];(858,483),[419];(858,484),[423];(858,486),[425];(858,488),[429];(858,489),[433];(858,491),[435];(858,492),[73];(858,493),[437];(858,512),[512];(858,522),[556];(858,523),[558];(858,525),[560];(858,570),[679];(858,571),[681];(858,572),[688];(858,573),[689;690];(858,574),[697];(858,575),[700];(858,576),[702];(858,577),[712];(858,579),[31];(858,581),[716];(858,585),[728];(858,586),[726];(858,587),[724];(858,588),[730];(858,589),[733];(858,590),[735];(858,594),[757];(858,595),[764;765];(858,596),[759];(858,597),[761];(858,598),[763];(858,599),[767];(858,603),[777];(858,604),[779];(858,609),[794];(858,610),[796];(858,611),[798];(858,612),[820];(858,614),[822];(858,615),[824];(858,616),[826];(858,617),[828];(858,618),[830];(858,622),[861];(858,624),[113];(858,627),[117];(858,628),[137];(858,629),[48];(858,630),[141];(858,631),[146];(858,633),[156];(858,639),[179];(858,640),[50];(858,650),[214];(858,651),[52];(858,652),[212];(858,653),[223];(858,654),[221];(858,655),[229;230];(858,656),[232];(858,657),[239];(858,658),[242];(858,659),[245];(858,660),[254;255];(858,661),[249];(858,663),[247];(858,664),[253];(858,665),[251];(858,666),[258;259];(858,667),[257];(858,668),[261];(858,669),[266;267];(858,670),[265];(858,671),[263];(858,672),[270];(858,674),[272];(858,675),[276];(858,676),[274];(858,679),[291;292];(858,681),[300];(858,682),[306];(858,683),[302];(858,685),[304];(858,686),[308];(858,687),[309;310];(858,688),[312];(858,689),[318];(858,690),[314];(858,691),[316];(858,692),[320];(858,694),[330];(858,696),[334];(858,708),[741];(858,709),[150];(858,710),[157];(858,711),[163];(858,713),[215];(858,714),[224];(858,717),[293];(858,718),[322];(858,722),[390];(858,730),[74];(858,732),[682];(858,734),[704];(858,735),[742];(858,738),[152];(858,739),[159];(858,740),[165];(858,742),[217];(858,743),[226];(858,746),[295];(858,747),[324];(858,750),[392];(858,760),[684];(858,762),[76];(858,763),[706];(858,764),[743;744];(858,768),[154];(858,769),[161];(858,770),[167];(858,772),[219];(858,773),[228];(858,776),[297];(858,777),[326];(858,780),[394];(858,790),[686];(858,792),[708];(858,793),[746];(858,795),[78];(858,797),[12];(858,806),[29];(859,12),[268];(859,32),[240];(859,34),[387];(859,67),[321];(859,153),[1021];(859,155),[1023];(859,159),[1028];(859,170),[1055];(859,454),[886];(859,455),[403;405];(859,456),[370];(859,471),[378];(859,476),[389];(859,478),[409];(859,479),[413];(859,480),[417];(859,483),[419];(859,484),[423];(859,486),[425];(859,488),[429];(859,489),[433];(859,491),[435];(859,493),[437];(859,512),[512];(859,585),[728];(859,586),[726];(859,587),[724];(859,588),[730];(859,622),[861];(859,650),[214];(859,652),[212];(859,653),[223];(859,654),[221];(859,655),[229;230];(859,656),[232];(859,657),[239];(859,658),[242];(859,659),[245];(859,660),[254;255];(859,661),[249];(859,663),[247];(859,664),[253];(859,665),[251];(859,666),[258;259];(859,667),[257];(859,668),[261];(859,669),[266;267];(859,670),[265];(859,671),[263];(859,672),[270];(859,674),[272];(859,675),[276];(859,676),[274];(859,679),[291;292];(859,681),[300];(859,682),[306];(859,683),[302];(859,685),[304];(859,686),[308];(859,687),[309;310];(859,688),[312];(859,689),[318];(859,690),[314];(859,691),[316];(859,692),[320];(859,694),[330];(859,696),[334];(859,709),[150];(859,713),[215];(859,714),[224];(859,717),[293];(859,718),[322];(859,722),[390];(859,732),[682];(859,738),[152];(859,742),[217];(859,743),[226];(859,746),[295];(859,747),[324];(859,750),[392];(859,760),[684];(859,768),[154];(859,772),[219];(859,773),[228];(859,776),[297];(859,777),[326];(859,780),[394];(859,790),[686];(859,797),[12];(860,380),[1396];(861,1),[44];(861,12),[268];(861,32),[240];(861,34),[387];(861,54),[133];(861,63),[28];(861,67),[321];(861,69),[55];(861,70),[53];(861,89),[929];(861,110),[961];(861,111),[962];(861,153),[1021];(861,155),[1023];(861,159),[1028];(861,170),[1055];(861,296),[1249];(861,302),[1256];(861,328),[1295];(861,359),[1350];(861,425),[1489];(861,450),[899];(861,454),[886];(861,455),[405];(861,456),[370];(861,464),[32];(861,465),[114;115];(861,466),[142];(861,471),[378];(861,476),[389];(861,478),[409];(861,479),[413];(861,480),[417];(861,481),[80];(861,483),[419];(861,484),[423];(861,486),[425];(861,488),[429];(861,489),[433];(861,491),[435];(861,492),[73];(861,493),[437];(861,512),[512];(861,522),[556];(861,523),[558];(861,525),[560];(861,570),[679];(861,571),[681];(861,572),[688];(861,573),[689;690];(861,574),[697];(861,575),[700];(861,576),[702];(861,577),[712];(861,579),[31];(861,581),[716];(861,585),[728];(861,586),[726];(861,587),[724];(861,588),[730];(861,589),[733];(861,590),[735];(861,594),[757];(861,595),[764;765];(861,596),[759];(861,597),[761];(861,598),[763];(861,599),[767];(861,603),[777];(861,604),[779];(861,609),[794];(861,610),[796];(861,611),[798];(861,612),[820];(861,614),[822];(861,615),[824];(861,616),[826];(861,617),[828];(861,618),[830];(861,622),[861];(861,624),[113];(861,627),[117];(861,628),[137];(861,629),[48];(861,630),[141];(861,631),[146];(861,633),[156];(861,639),[179];(861,640),[50];(861,650),[214];(861,651),[52];(861,652),[212];(861,653),[223];(861,654),[221];(861,655),[229;230];(861,656),[232];(861,657),[239];(861,658),[242];(861,659),[245];(861,660),[254;255];(861,661),[249];(861,663),[247];(861,664),[253];(861,665),[251];(861,666),[258;259];(861,667),[257];(861,668),[261];(861,669),[266;267];(861,670),[265];(861,671),[263];(861,672),[270];(861,674),[272];(861,675),[276];(861,676),[274];(861,679),[291;292];(861,681),[300];(861,682),[306];(861,683),[302];(861,685),[304];(861,686),[308];(861,687),[309;310];(861,688),[312];(861,689),[318];(861,690),[314];(861,691),[316];(861,692),[320];(861,694),[330];(861,696),[334];(861,708),[741];(861,709),[150];(861,710),[157];(861,711),[163];(861,713),[215];(861,714),[224];(861,717),[293];(861,718),[322];(861,722),[390];(861,730),[74];(861,732),[682];(861,734),[704];(861,735),[742];(861,738),[152];(861,739),[159];(861,740),[165];(861,742),[217];(861,743),[226];(861,746),[295];(861,747),[324];(861,750),[392];(861,760),[684];(861,762),[76];(861,763),[706];(861,764),[743;744];(861,768),[154];(861,769),[161];(861,770),[167];(861,772),[219];(861,773),[228];(861,776),[297];(861,777),[326];(861,780),[394];(861,790),[686];(861,792),[708];(861,793),[746];(861,795),[78];(861,797),[12];(861,806),[29];(862,453),[918];(863,72),[813];(863,355),[1342];(863,433),[1506];(863,513),[88];(863,524),[89];(863,634),[169];(863,737),[894];(863,766),[895];(864,72),[814];(864,355),[1342];(864,433),[1506];(864,513),[88];(864,524),[89];(864,634),[169];(864,737),[894];(864,766),[895];(865,1),[40];(865,12),[268];(865,21),[162];(865,32),[240];(865,34),[387];(865,54),[131];(865,63),[28];(865,67),[321];(865,110),[961];(865,111),[962];(865,153),[1021];(865,155),[1023];(865,159),[1028];(865,170),[1055];(865,296),[1249];(865,302),[1256];(865,328),[1295];(865,425),[1489];(865,450),[899];(865,454),[886];(865,455),[405];(865,456),[370];(865,464),[32];(865,465),[114;115];(865,466),[142];(865,471),[378];(865,476),[389];(865,478),[409];(865,479),[413];(865,480),[417];(865,481),[80];(865,483),[419];(865,484),[423];(865,486),[425];(865,488),[429];(865,489),[433];(865,491),[435];(865,492),[73];(865,493),[437];(865,512),[512];(865,522),[556];(865,523),[558];(865,525),[560];(865,570),[679];(865,571),[681];(865,572),[688];(865,573),[689;690];(865,574),[697];(865,575),[700];(865,576),[702];(865,577),[712];(865,579),[31];(865,581),[716];(865,585),[728];(865,586),[726];(865,587),[724];(865,588),[730];(865,589),[733];(865,590),[735];(865,594),[757];(865,595),[764;765];(865,596),[759];(865,597),[761];(865,598),[763];(865,599),[767];(865,603),[777];(865,604),[779];(865,609),[794];(865,610),[796];(865,611),[798];(865,612),[820];(865,614),[822];(865,615),[824];(865,616),[826];(865,617),[828];(865,618),[830];(865,622),[861];(865,624),[113];(865,627),[117];(865,628),[137];(865,629),[48];(865,630),[141];(865,631),[146];(865,633),[156];(865,639),[179];(865,640),[50];(865,650),[214];(865,651),[52];(865,652),[212];(865,653),[223];(865,654),[221];(865,655),[229;230];(865,656),[232];(865,657),[239];(865,658),[242];(865,659),[245];(865,660),[254;255];(865,661),[249];(865,663),[247];(865,664),[253];(865,665),[251];(865,666),[258;259];(865,667),[257];(865,668),[261];(865,669),[266;267];(865,670),[265];(865,671),[263];(865,672),[270];(865,674),[272];(865,675),[276];(865,676),[274];(865,679),[291;292];(865,681),[300];(865,682),[306];(865,683),[302];(865,685),[304];(865,686),[308];(865,687),[309;310];(865,688),[312];(865,689),[318];(865,690),[314];(865,691),[316];(865,692),[320];(865,694),[330];(865,696),[334];(865,708),[741];(865,709),[150];(865,710),[157];(865,711),[163];(865,713),[215];(865,714),[224];(865,717),[293];(865,718),[322];(865,722),[390];(865,730),[74];(865,732),[682];(865,734),[704];(865,735),[742];(865,738),[152];(865,739),[159];(865,740),[165];(865,742),[217];(865,743),[226];(865,746),[295];(865,747),[324];(865,750),[392];(865,760),[684];(865,762),[76];(865,763),[706];(865,764),[743;744];(865,768),[154];(865,769),[161];(865,770),[167];(865,772),[219];(865,773),[228];(865,776),[297];(865,777),[326];(865,780),[394];(865,790),[686];(865,792),[708];(865,793),[746];(865,795),[78];(865,797),[12];(865,806),[29];(866,12),[268];(866,32),[240];(866,34),[387];(866,67),[321];(866,125),[984];(866,127),[987];(866,153),[1021];(866,155),[1023];(866,159),[1028];(866,170),[1055];(866,255),[1188];(866,296),[1249];(866,302),[1255];(866,303),[1260];(866,328),[1295];(866,405),[1448];(866,406),[1451];(866,408),[1455];(866,409),[1458];(866,413),[1466];(866,419),[1481];(866,425),[1489];(866,454),[886];(866,455),[405];(866,456),[370];(866,471),[378];(866,476),[389];(866,478),[409];(866,479),[413];(866,480),[417];(866,483),[419];(866,484),[423];(866,486),[425];(866,488),[429];(866,489),[433];(866,491),[435];(866,493),[437];(866,512),[512];(866,526),[564];(866,527),[566];(866,528),[569];(866,529),[568];(866,570),[679];(866,571),[681];(866,572),[688];(866,573),[689];(866,575),[700];(866,576),[702];(866,577),[712];(866,581),[716];(866,585),[728];(866,586),[726];(866,587),[724];(866,588),[730];(866,594),[757];(866,595),[764];(866,596),[759];(866,598),[762];(866,612),[820];(866,614),[822];(866,615),[824];(866,616),[826];(866,617),[828];(866,618),[830];(866,622),[861];(866,641),[185];(866,642),[190];(866,643),[186];(866,650),[214];(866,652),[212];(866,653),[223];(866,654),[221];(866,655),[229;230];(866,656),[232];(866,657),[239];(866,658),[242];(866,659),[245];(866,660),[254;255];(866,661),[249];(866,663),[247];(866,664),[253];(866,665),[251];(866,666),[258;259];(866,667),[257];(866,668),[261];(866,669),[266;267];(866,670),[265];(866,671),[263];(866,672),[270];(866,674),[272];(866,675),[276];(866,676),[274];(866,679),[291;292];(866,681),[300];(866,682),[306];(866,683),[302];(866,685),[304];(866,686),[308];(866,687),[309;310];(866,688),[312];(866,689),[318];(866,690),[314];(866,691),[316];(866,692),[320];(866,694),[330];(866,696),[334];(866,708),[741];(866,709),[150];(866,713),[215];(866,714),[224];(866,717),[293];(866,718),[322];(866,722),[390];(866,732),[682];(866,733),[691];(866,734),[704];(866,735),[742];(866,738),[152];(866,742),[217];(866,743),[226];(866,746),[295];(866,747),[324];(866,750),[392];(866,760),[684];(866,761),[692];(866,763),[706];(866,764),[743];(866,768),[154];(866,772),[219];(866,773),[228];(866,776),[297];(866,777),[326];(866,780),[394];(866,790),[686];(866,792),[708];(866,797),[12];(867,293),[1240];(868,12),[268];(868,24),[703];(868,32),[240];(868,34),[387];(868,43),[514;515];(868,51),[373;374;375;376];(868,61),[347];(868,66),[364];(868,67),[321];(868,73),[869];(868,79),[399];(868,153),[1021];(868,155),[1023];(868,159),[1028];(868,170),[1055];(868,181),[1069];(868,189),[1083];(868,193),[1091];(868,195),[1094];(868,204),[1105];(868,303),[1258];(868,308),[1266];(868,309),[1268];(868,321),[1286];(868,328),[1295];(868,355),[1341];(868,381),[1397];(868,382),[1400];(868,385),[1407];(868,387),[1411];(868,419),[1479];(868,420),[1482];(868,421),[1484];(868,424),[1488];(868,425),[1491];(868,433),[1505];(868,437),[1517];(868,444),[1534];(868,454),[886];(868,455),[405];(868,456),[370];(868,471),[378];(868,476),[389];(868,478),[409];(868,479),[413];(868,480),[417];(868,483),[419];(868,484),[423];(868,486),[425];(868,488),[429];(868,489),[433];(868,491),[435];(868,493),[437];(868,512),[512];(868,517),[532];(868,543),[606];(868,555),[629];(868,564),[660];(868,577),[712];(868,585),[728];(868,586),[726];(868,587),[724];(868,588),[730];(868,594),[757];(868,595),[764];(868,596),[759];(868,600),[769];(868,622),[861];(868,635),[171];(868,636),[173];(868,637),[175];(868,650),[214];(868,652),[212];(868,653),[223];(868,654),[221];(868,655),[229;230];(868,656),[232];(868,657),[239];(868,658),[242];(868,659),[245];(868,660),[254;255];(868,661),[249];(868,663),[247];(868,664),[253];(868,665),[251];(868,666),[258;259];(868,667),[257];(868,668),[261];(868,669),[266;267];(868,670),[265];(868,671),[263];(868,672),[270];(868,674),[272];(868,675),[276];(868,676),[274];(868,679),[291;292];(868,681),[300];(868,682),[306];(868,683),[302];(868,685),[304];(868,686),[308];(868,687),[309;310];(868,688),[312];(868,689),[318];(868,690),[314];(868,691),[316];(868,692),[320];(868,694),[330];(868,696),[334];(868,698),[338];(868,700),[348];(868,704),[366];(868,705),[368];(868,708),[741];(868,709),[150];(868,713),[215];(868,714),[224];(868,717),[293];(868,718),[322];(868,721),[359];(868,722),[390];(868,732),[682];(868,733),[691];(868,734),[704];(868,735),[742];(868,736),[747];(868,737),[894];(868,738),[151;152];(868,742),[217];(868,743),[226];(868,746),[295];(868,747),[323;324];(868,749),[360];(868,750),[391;392];(868,760),[684];(868,761),[692];(868,763),[705];(868,764),[743];(868,765),[748];(868,766),[895];(868,768),[154];(868,772),[219];(868,773),[228];(868,776),[297];(868,777),[326];(868,780),[394];(868,790),[686];(868,797),[12];(868,802),[528;529];(869,12),[268];(869,32),[240];(869,34),[387];(869,67),[321];(869,136),[1000];(869,153),[1021];(869,155),[1023];(869,159),[1028];(869,170),[1055];(869,191),[1088];(869,287),[1231];(869,454),[886];(869,455),[405];(869,456),[370];(869,471),[378];(869,476),[389];(869,478),[409];(869,479),[413];(869,480),[417];(869,483),[419];(869,484),[423];(869,486),[425];(869,488),[429];(869,489),[433];(869,491),[435];(869,493),[437];(869,512),[512];(869,559),[647];(869,585),[728];(869,586),[726];(869,587),[724];(869,588),[730];(869,622),[861];(869,650),[214];(869,652),[211;212];(869,653),[223];(869,654),[221];(869,655),[229;230];(869,656),[232];(869,657),[239];(869,658),[242];(869,659),[245];(869,660),[254;255];(869,661),[249];(869,663),[247];(869,664),[253];(869,665),[251];(869,666),[258;259];(869,667),[257];(869,668),[261];(869,669),[266;267];(869,670),[265];(869,671),[263];(869,672),[270];(869,674),[272];(869,675),[276];(869,676),[274];(869,679),[291;292];(869,681),[300];(869,682),[306];(869,683),[302];(869,685),[304];(869,686),[308];(869,687),[309;310];(869,688),[312];(869,689),[318];(869,690),[314];(869,691),[316];(869,692),[320];(869,694),[330];(869,696),[334];(869,703),[355];(869,709),[150];(869,713),[215];(869,714),[224];(869,717),[293];(869,718),[322];(869,722),[390];(869,732),[682];(869,738),[152];(869,742),[217];(869,743),[226];(869,746),[295];(869,747),[324];(869,750),[392];(869,760),[684];(869,768),[154];(869,772),[219];(869,773),[228];(869,776),[297];(869,777),[326];(869,780),[394];(869,790),[686];(869,797),[12];(870,173),[1059];(870,186),[1076];(870,681),[299];(870,698),[337];(871,1),[36];(871,12),[268];(871,25),[81];(871,32),[240];(871,34),[387];(871,54),[123];(871,63),[28];(871,67),[321];(871,110),[961];(871,111),[962];(871,153),[1021];(871,155),[1023];(871,159),[1028];(871,170),[1055];(871,296),[1249];(871,302),[1256];(871,328),[1295];(871,425),[1489];(871,450),[899];(871,454),[886];(871,455),[405];(871,456),[370];(871,464),[32];(871,465),[114;115];(871,466),[142];(871,471),[378];(871,476),[389];(871,478),[409];(871,479),[413];(871,480),[417];(871,481),[80];(871,483),[419];(871,484),[423];(871,486),[425];(871,488),[429];(871,489),[433];(871,491),[435];(871,492),[73];(871,493),[437];(871,512),[512];(871,522),[556];(871,523),[558];(871,525),[560];(871,570),[679];(871,571),[681];(871,572),[688];(871,573),[689;690];(871,574),[697];(871,575),[700];(871,576),[702];(871,577),[712];(871,579),[31];(871,581),[716];(871,585),[728];(871,586),[726];(871,587),[724];(871,588),[730];(871,589),[733];(871,590),[735];(871,594),[757];(871,595),[764;765];(871,596),[759];(871,597),[761];(871,598),[763];(871,599),[767];(871,603),[777];(871,604),[779];(871,609),[794];(871,610),[796];(871,611),[798];(871,612),[820];(871,614),[822];(871,615),[824];(871,616),[826];(871,617),[828];(871,618),[830];(871,622),[861];(871,624),[113];(871,627),[117];(871,628),[137];(871,629),[48];(871,630),[141];(871,631),[146];(871,633),[156];(871,639),[179];(871,640),[50];(871,650),[214];(871,651),[52];(871,652),[212];(871,653),[223];(871,654),[221];(871,655),[229;230];(871,656),[232];(871,657),[239];(871,658),[242];(871,659),[245];(871,660),[254;255];(871,661),[249];(871,663),[247];(871,664),[253];(871,665),[251];(871,666),[258;259];(871,667),[257];(871,668),[261];(871,669),[266;267];(871,670),[265];(871,671),[263];(871,672),[270];(871,674),[272];(871,675),[276];(871,676),[274];(871,679),[291;292];(871,681),[300];(871,682),[306];(871,683),[302];(871,685),[304];(871,686),[308];(871,687),[309;310];(871,688),[312];(871,689),[318];(871,690),[314];(871,691),[316];(871,692),[320];(871,694),[330];(871,696),[334];(871,708),[741];(871,709),[150];(871,710),[157];(871,711),[163];(871,713),[215];(871,714),[224];(871,717),[293];(871,718),[322];(871,722),[390];(871,730),[74];(871,732),[682];(871,734),[704];(871,735),[742];(871,738),[152];(871,739),[159];(871,740),[165];(871,742),[217];(871,743),[226];(871,746),[295];(871,747),[324];(871,750),[392];(871,760),[684];(871,762),[76];(871,763),[706];(871,764),[743;744];(871,768),[154];(871,769),[161];(871,770),[167];(871,772),[219];(871,773),[228];(871,776),[297];(871,777),[326];(871,780),[394];(871,790),[686];(871,792),[708];(871,793),[746];(871,795),[78];(871,797),[12];(871,806),[29];(872,93),[937];(872,673),[65];(872,684),[66];(873,360),[1352];(875,12),[268];(875,32),[240];(875,34),[387];(875,67),[321];(875,112),[963];(875,153),[1021];(875,155),[1023];(875,159),[1028];(875,170),[1055];(875,245),[1176];(875,296),[1249];(875,328),[1295];(875,425),[1489];(875,454),[886];(875,455),[405];(875,456),[370];(875,458),[535];(875,465),[115];(875,471),[378];(875,476),[389];(875,478),[409];(875,479),[413];(875,480),[417];(875,481),[80];(875,483),[419];(875,484),[423];(875,486),[425];(875,488),[429];(875,489),[433];(875,491),[435];(875,492),[73];(875,493),[437];(875,512),[512];(875,517),[533];(875,518),[536];(875,522),[556];(875,523),[558];(875,525),[560];(875,570),[679];(875,571),[681];(875,572),[688];(875,573),[690];(875,574),[697];(875,575),[700];(875,576),[702];(875,577),[712];(875,581),[716];(875,585),[728];(875,586),[726];(875,587),[724];(875,588),[730];(875,589),[733];(875,590),[735];(875,594),[757];(875,595),[764;765];(875,596),[759];(875,597),[761];(875,598),[763];(875,599),[767];(875,603),[777];(875,604),[779];(875,609),[794];(875,610),[796];(875,611),[798];(875,612),[820];(875,614),[822];(875,615),[824];(875,616),[826];(875,617),[828];(875,618),[830];(875,622),[861];(875,624),[113];(875,627),[117];(875,628),[137];(875,629),[48];(875,631),[145;146];(875,633),[156];(875,639),[179];(875,640),[50];(875,650),[214];(875,651),[52];(875,652),[212];(875,653),[223];(875,654),[221];(875,655),[229;230];(875,656),[232];(875,657),[239];(875,658),[242];(875,659),[245];(875,660),[254;255];(875,661),[249];(875,663),[247];(875,664),[253];(875,665),[251];(875,666),[258;259];(875,667),[257];(875,668),[261];(875,669),[266;267];(875,670),[265];(875,671),[263];(875,672),[270];(875,674),[272];(875,675),[276];(875,676),[274];(875,679),[291;292];(875,681),[300];(875,682),[306];(875,683),[302];(875,685),[304];(875,686),[308];(875,687),[309;310];(875,688),[312];(875,689),[318];(875,690),[314];(875,691),[316];(875,692),[320];(875,694),[330];(875,696),[334];(875,708),[741];(875,709),[150];(875,710),[157];(875,711),[163];(875,713),[215];(875,714),[224];(875,717),[293];(875,718),[322];(875,722),[390];(875,730),[74];(875,732),[682];(875,734),[704];(875,735),[742];(875,738),[152];(875,739),[159];(875,740),[165];(875,742),[217];(875,743),[226];(875,746),[295];(875,747),[324];(875,750),[392];(875,760),[684];(875,762),[76];(875,763),[706];(875,764),[743;744];(875,768),[154];(875,769),[161];(875,770),[167];(875,772),[219];(875,773),[228];(875,776),[297];(875,777),[326];(875,780),[394];(875,790),[686];(875,792),[708];(875,793),[746];(875,795),[78];(875,797),[12];(876,12),[268];(876,32),[240];(876,34),[387];(876,67),[321];(876,153),[1021];(876,155),[1023];(876,159),[1028];(876,170),[1055];(876,296),[1249];(876,328),[1295];(876,425),[1489];(876,454),[886];(876,455),[405];(876,456),[370];(876,458),[535];(876,465),[115];(876,466),[143];(876,471),[378];(876,476),[389];(876,478),[409];(876,479),[413];(876,480),[417];(876,481),[80];(876,483),[419];(876,484),[423];(876,486),[425];(876,488),[429];(876,489),[433];(876,491),[435];(876,492),[73];(876,493),[437];(876,512),[512];(876,517),[533];(876,518),[537];(876,522),[556];(876,523),[558];(876,525),[560];(876,570),[679];(876,571),[681];(876,572),[688];(876,573),[690];(876,574),[697];(876,575),[700];(876,576),[702];(876,577),[712];(876,581),[716];(876,585),[728];(876,586),[726];(876,587),[724];(876,588),[730];(876,589),[733];(876,590),[735];(876,594),[757];(876,595),[764;765];(876,596),[759];(876,597),[761];(876,598),[763];(876,599),[767];(876,603),[777];(876,604),[779];(876,609),[794];(876,610),[796];(876,611),[798];(876,612),[820];(876,614),[822];(876,615),[824];(876,616),[826];(876,617),[828];(876,618),[830];(876,622),[861];(876,624),[113];(876,627),[117];(876,628),[137];(876,629),[48];(876,630),[141];(876,631),[146];(876,633),[156];(876,639),[179];(876,640),[50];(876,650),[214];(876,651),[52];(876,652),[212];(876,653),[223];(876,654),[221];(876,655),[229;230];(876,656),[232];(876,657),[239];(876,658),[242];(876,659),[245];(876,660),[254;255];(876,661),[249];(876,663),[247];(876,664),[253];(876,665),[251];(876,666),[258;259];(876,667),[257];(876,668),[261];(876,669),[266;267];(876,670),[265];(876,671),[263];(876,672),[270];(876,674),[272];(876,675),[276];(876,676),[274];(876,679),[291;292];(876,681),[300];(876,682),[306];(876,683),[302];(876,685),[304];(876,686),[308];(876,687),[309;310];(876,688),[312];(876,689),[318];(876,690),[314];(876,691),[316];(876,692),[320];(876,694),[330];(876,696),[334];(876,708),[741];(876,709),[150];(876,710),[157];(876,711),[163];(876,713),[215];(876,714),[224];(876,717),[293];(876,718),[322];(876,722),[390];(876,730),[74];(876,732),[682];(876,734),[704];(876,735),[742];(876,738),[152];(876,739),[159];(876,740),[165];(876,742),[217];(876,743),[226];(876,746),[295];(876,747),[324];(876,750),[392];(876,760),[684];(876,762),[76];(876,763),[706];(876,764),[743;744];(876,768),[154];(876,769),[161];(876,770),[167];(876,772),[219];(876,773),[228];(876,776),[297];(876,777),[326];(876,780),[394];(876,790),[686];(876,792),[708];(876,793),[746];(876,795),[78];(876,797),[12];(878,12),[268];(878,32),[240];(878,34),[387];(878,67),[321];(878,153),[1021];(878,155),[1023];(878,159),[1028];(878,170),[1055];(878,199),[1100];(878,454),[886];(878,455),[405];(878,456),[370];(878,471),[377;378];(878,476),[389];(878,478),[409];(878,479),[413];(878,480),[417];(878,483),[419];(878,484),[423];(878,486),[425];(878,488),[429];(878,489),[433];(878,491),[435];(878,493),[437];(878,512),[512];(878,585),[728];(878,586),[726];(878,587),[724];(878,588),[730];(878,622),[861];(878,650),[214];(878,652),[212];(878,653),[223];(878,654),[221];(878,655),[229;230];(878,656),[232];(878,657),[239];(878,658),[242];(878,659),[245];(878,660),[254;255];(878,661),[249];(878,663),[247];(878,664),[253];(878,665),[251];(878,666),[258;259];(878,667),[257];(878,668),[261];(878,669),[266;267];(878,670),[265];(878,671),[263];(878,672),[270];(878,674),[272];(878,675),[276];(878,676),[274];(878,679),[291;292];(878,681),[300];(878,682),[306];(878,683),[302];(878,685),[304];(878,686),[308];(878,687),[309;310];(878,688),[312];(878,689),[318];(878,690),[314];(878,691),[316];(878,692),[320];(878,694),[330];(878,696),[334];(878,709),[150];(878,713),[215];(878,714),[224];(878,717),[293];(878,718),[322];(878,722),[390];(878,732),[682];(878,738),[152];(878,742),[217];(878,743),[226];(878,746),[295];(878,747),[324];(878,750),[392];(878,760),[684];(878,768),[154];(878,772),[219];(878,773),[228];(878,776),[297];(878,777),[326];(878,780),[394];(878,790),[686];(878,797),[12];(879,12),[268];(879,32),[240];(879,34),[387];(879,67),[321];(879,153),[1021];(879,155),[1023];(879,159),[1028];(879,170),[1055;1056];(879,378),[1392];(879,454),[886];(879,455),[405];(879,456),[370];(879,471),[378];(879,476),[389];(879,478),[409];(879,479),[413];(879,480),[417];(879,483),[419];(879,484),[423];(879,486),[425];(879,488),[429];(879,489),[433];(879,491),[435];(879,493),[437];(879,512),[512];(879,585),[728];(879,586),[726];(879,587),[724];(879,588),[730];(879,622),[861];(879,650),[214];(879,652),[212];(879,653),[223];(879,654),[221];(879,655),[229;230];(879,656),[232];(879,657),[239];(879,658),[242];(879,659),[245];(879,660),[254;255];(879,661),[249];(879,663),[247];(879,664),[253];(879,665),[251];(879,666),[258;259];(879,667),[257];(879,668),[261];(879,669),[266;267];(879,670),[265];(879,671),[263];(879,672),[270];(879,674),[272];(879,675),[276];(879,676),[274];(879,679),[291;292];(879,681),[300];(879,682),[306];(879,683),[302];(879,685),[304];(879,686),[308];(879,687),[309;310];(879,688),[312];(879,689),[318];(879,690),[314];(879,691),[316];(879,692),[320];(879,694),[330];(879,696),[334];(879,709),[150];(879,713),[215];(879,714),[224];(879,717),[293];(879,718),[322];(879,722),[390];(879,732),[682];(879,738),[152];(879,742),[217];(879,743),[226];(879,746),[295];(879,747),[324];(879,750),[392];(879,760),[684];(879,768),[154];(879,772),[219];(879,773),[228];(879,776),[297];(879,777),[326];(879,780),[394];(879,790),[686];(879,797),[12];(880,12),[268];(880,26),[831];(880,27),[771];(880,28),[780];(880,29),[739];(880,30),[736;737;738];(880,32),[240];(880,34),[387];(880,54),[132];(880,67),[321];(880,110),[961];(880,111),[962];(880,153),[1021];(880,155),[1023];(880,159),[1028];(880,170),[1055];(880,296),[1249];(880,323),[1289];(880,328),[1295];(880,331),[1299];(880,335),[1304];(880,343),[1316];(880,425),[1489];(880,454),[886];(880,455),[405];(880,456),[370];(880,465),[114;115];(880,466),[142];(880,471),[378];(880,476),[389];(880,478),[409];(880,479),[413];(880,480),[417];(880,481),[80];(880,483),[419];(880,484),[423];(880,486),[425];(880,488),[429];(880,489),[433];(880,491),[435];(880,492),[73];(880,493),[437];(880,512),[512];(880,522),[556];(880,523),[558];(880,525),[560];(880,570),[679];(880,571),[681];(880,572),[688];(880,573),[690];(880,574),[697];(880,575),[700];(880,576),[702];(880,577),[712];(880,581),[716];(880,585),[728];(880,586),[726];(880,587),[724];(880,588),[730];(880,589),[733];(880,590),[735];(880,592),[752];(880,594),[757];(880,595),[764;765];(880,596),[759];(880,597),[761];(880,598),[763];(880,599),[767];(880,603),[777];(880,604),[779];(880,609),[794];(880,610),[796];(880,611),[798];(880,612),[820];(880,614),[822];(880,615),[824];(880,616),[826];(880,617),[828];(880,618),[830];(880,622),[861];(880,624),[113];(880,627),[117];(880,628),[137];(880,629),[48];(880,630),[141];(880,631),[146];(880,633),[156];(880,639),[179];(880,640),[50];(880,650),[214];(880,651),[52];(880,652),[212];(880,653),[223];(880,654),[221];(880,655),[229;230];(880,656),[232];(880,657),[239];(880,658),[242];(880,659),[245];(880,660),[254;255];(880,661),[249];(880,663),[247];(880,664),[253];(880,665),[251];(880,666),[258;259];(880,667),[257];(880,668),[261];(880,669),[266;267];(880,670),[265];(880,671),[263];(880,672),[270];(880,674),[272];(880,675),[276];(880,676),[274];(880,679),[291;292];(880,681),[300];(880,682),[306];(880,683),[302];(880,685),[304];(880,686),[308];(880,687),[309;310];(880,688),[312];(880,689),[318];(880,690),[314];(880,691),[316];(880,692),[320];(880,694),[330];(880,696),[334];(880,708),[741];(880,709),[150];(880,710),[157];(880,711),[163];(880,713),[215];(880,714),[224];(880,717),[293];(880,718),[322];(880,722),[390];(880,730),[74];(880,732),[682];(880,734),[704];(880,735),[742];(880,738),[152];(880,739),[159];(880,740),[165];(880,742),[217];(880,743),[226];(880,746),[295];(880,747),[324];(880,750),[392];(880,760),[684];(880,762),[76];(880,763),[706];(880,764),[743;744];(880,768),[154];(880,769),[161];(880,770),[167];(880,772),[219];(880,773),[228];(880,776),[297];(880,777),[326];(880,780),[394];(880,790),[686];(880,792),[708];(880,793),[746];(880,795),[78];(880,797),[12];(880,799),[106;107];(881,12),[268];(881,26),[831];(881,27),[771];(881,28),[780];(881,29),[739];(881,30),[736;737;738];(881,32),[240];(881,34),[387];(881,54),[132];(881,67),[321];(881,110),[961];(881,111),[962];(881,153),[1021];(881,155),[1023];(881,159),[1028];(881,170),[1055];(881,296),[1249];(881,323),[1288];(881,328),[1295];(881,331),[1298];(881,335),[1303];(881,343),[1315];(881,425),[1489];(881,454),[886];(881,455),[405];(881,456),[370];(881,465),[114;115];(881,466),[142];(881,471),[378];(881,476),[389];(881,478),[409];(881,479),[413];(881,480),[417];(881,481),[80];(881,483),[419];(881,484),[423];(881,486),[425];(881,488),[429];(881,489),[433];(881,491),[435];(881,492),[73];(881,493),[437];(881,512),[512];(881,522),[556];(881,523),[558];(881,525),[560];(881,570),[679];(881,571),[681];(881,572),[688];(881,573),[690];(881,574),[697];(881,575),[700];(881,576),[702];(881,577),[712];(881,581),[716];(881,585),[728];(881,586),[726];(881,587),[724];(881,588),[730];(881,589),[733];(881,590),[735];(881,592),[752];(881,594),[757];(881,595),[764;765];(881,596),[759];(881,597),[761];(881,598),[763];(881,599),[767];(881,603),[777];(881,604),[779];(881,609),[794];(881,610),[796];(881,611),[798];(881,612),[820];(881,614),[822];(881,615),[824];(881,616),[826];(881,617),[828];(881,618),[830];(881,622),[861];(881,624),[113];(881,627),[117];(881,628),[137];(881,629),[48];(881,630),[141];(881,631),[146];(881,633),[156];(881,639),[179];(881,640),[50];(881,650),[214];(881,651),[52];(881,652),[212];(881,653),[223];(881,654),[221];(881,655),[229;230];(881,656),[232];(881,657),[239];(881,658),[242];(881,659),[245];(881,660),[254;255];(881,661),[249];(881,663),[247];(881,664),[253];(881,665),[251];(881,666),[258;259];(881,667),[257];(881,668),[261];(881,669),[266;267];(881,670),[265];(881,671),[263];(881,672),[270];(881,674),[272];(881,675),[276];(881,676),[274];(881,679),[291;292];(881,681),[300];(881,682),[306];(881,683),[302];(881,685),[304];(881,686),[308];(881,687),[309;310];(881,688),[312];(881,689),[318];(881,690),[314];(881,691),[316];(881,692),[320];(881,694),[330];(881,696),[334];(881,708),[741];(881,709),[150];(881,710),[157];(881,711),[163];(881,713),[215];(881,714),[224];(881,717),[293];(881,718),[322];(881,722),[390];(881,730),[74];(881,732),[682];(881,734),[704];(881,735),[742];(881,738),[152];(881,739),[159];(881,740),[165];(881,742),[217];(881,743),[226];(881,746),[295];(881,747),[324];(881,750),[392];(881,760),[684];(881,762),[76];(881,763),[706];(881,764),[743;744];(881,768),[154];(881,769),[161];(881,770),[167];(881,772),[219];(881,773),[228];(881,776),[297];(881,777),[326];(881,780),[394];(881,790),[686];(881,792),[708];(881,793),[746];(881,795),[78];(881,797),[12];(881,799),[106;107];(882,12),[268];(882,24),[703];(882,32),[240];(882,34),[387];(882,43),[514;515];(882,51),[372;373;374;375;376];(882,66),[364];(882,67),[321];(882,73),[865];(882,79),[399];(882,153),[1021];(882,155),[1023];(882,159),[1028];(882,170),[1055];(882,181),[1069];(882,193),[1091];(882,195),[1094];(882,204),[1105];(882,303),[1258];(882,308),[1266];(882,309),[1268];(882,321),[1286];(882,328),[1295];(882,355),[1341];(882,381),[1397];(882,382),[1400];(882,385),[1407];(882,387),[1411];(882,419),[1479];(882,420),[1482];(882,421),[1484];(882,424),[1488];(882,425),[1491];(882,433),[1505];(882,437),[1517];(882,444),[1534];(882,454),[886];(882,455),[405];(882,456),[370];(882,471),[378];(882,476),[389];(882,478),[409];(882,479),[413];(882,480),[417];(882,483),[419];(882,484),[423];(882,486),[425];(882,488),[429];(882,489),[433];(882,491),[435];(882,493),[437];(882,512),[512];(882,517),[532];(882,543),[606];(882,555),[629];(882,564),[660];(882,577),[712];(882,585),[728];(882,586),[726];(882,587),[724];(882,588),[730];(882,594),[757];(882,595),[764];(882,596),[759];(882,600),[769];(882,622),[861];(882,635),[171];(882,636),[173];(882,637),[175];(882,650),[214];(882,652),[212];(882,653),[223];(882,654),[221];(882,655),[229;230];(882,656),[232];(882,657),[239];(882,658),[242];(882,659),[245];(882,660),[254;255];(882,661),[249];(882,663),[247];(882,664),[253];(882,665),[251];(882,666),[258;259];(882,667),[257];(882,668),[261];(882,669),[266;267];(882,670),[265];(882,671),[263];(882,672),[270];(882,674),[272];(882,675),[276];(882,676),[274];(882,679),[291;292];(882,681),[300];(882,682),[306];(882,683),[302];(882,685),[304];(882,686),[308];(882,687),[309;310];(882,688),[312];(882,689),[318];(882,690),[314];(882,691),[316];(882,692),[320];(882,694),[330];(882,696),[334];(882,698),[338];(882,700),[348];(882,704),[366];(882,705),[368];(882,708),[741];(882,709),[150];(882,713),[215];(882,714),[224];(882,717),[293];(882,718),[322];(882,721),[359];(882,722),[390];(882,732),[682];(882,733),[691];(882,734),[704];(882,735),[742];(882,736),[747];(882,737),[894];(882,738),[151;152];(882,742),[217];(882,743),[226];(882,746),[295];(882,747),[323;324];(882,749),[360];(882,750),[391;392];(882,760),[684];(882,761),[692];(882,763),[705];(882,764),[743];(882,765),[748];(882,766),[895];(882,768),[154];(882,772),[219];(882,773),[228];(882,776),[297];(882,777),[326];(882,780),[394];(882,790),[686];(882,797),[12];(882,802),[528;529];(883,58),[277];(883,164),[1035];(883,715),[233];(883,744),[234];(884,147),[1013];(885,58),[277];(885,164),[1044];(885,715),[233];(885,744),[234];(886,78),[503];(886,240),[1168];(886,463),[676];(886,509),[505];(887,93),[936];(887,673),[65];(887,684),[66];(888,12),[268];(888,24),[703];(888,31),[853];(888,32),[240];(888,34),[387];(888,43),[514;515];(888,51),[373;374;375;376];(888,54),[119];(888,66),[364];(888,67),[321];(888,73),[862];(888,79),[399];(888,110),[961];(888,111),[962];(888,153),[1021];(888,155),[1023];(888,159),[1028];(888,170),[1055];(888,181),[1069];(888,193),[1091];(888,195),[1094];(888,204),[1105];(888,296),[1249];(888,303),[1258];(888,308),[1266];(888,309),[1268];(888,321),[1286];(888,328),[1295];(888,355),[1341];(888,381),[1397];(888,382),[1400];(888,385),[1407];(888,387),[1411];(888,419),[1479];(888,420),[1482];(888,421),[1484];(888,424),[1488];(888,425),[1489;1491];(888,433),[1505];(888,437),[1517];(888,444),[1534];(888,454),[886];(888,455),[405];(888,456),[370];(888,465),[114;115];(888,466),[142];(888,471),[378];(888,476),[389];(888,478),[409];(888,479),[413];(888,480),[417];(888,481),[80];(888,483),[419];(888,484),[423];(888,486),[425];(888,488),[429];(888,489),[433];(888,491),[435];(888,492),[73];(888,493),[437];(888,512),[512];(888,517),[532];(888,522),[556];(888,523),[558];(888,525),[560];(888,543),[606];(888,555),[629];(888,564),[660];(888,570),[679];(888,571),[681];(888,572),[688];(888,573),[690];(888,574),[697];(888,575),[700];(888,576),[702];(888,577),[712];(888,581),[716];(888,585),[728];(888,586),[726];(888,587),[724];(888,588),[730];(888,589),[733];(888,590),[735];(888,594),[757];(888,595),[764;765];(888,596),[759];(888,597),[761];(888,598),[763];(888,599),[767];(888,600),[769];(888,603),[777];(888,604),[779];(888,609),[794];(888,610),[796];(888,611),[798];(888,612),[820];(888,614),[822];(888,615),[824];(888,616),[826];(888,617),[828];(888,618),[830];(888,622),[861];(888,624),[113];(888,627),[117];(888,628),[137];(888,629),[48];(888,630),[141];(888,631),[146];(888,633),[156];(888,635),[171];(888,636),[173];(888,637),[175];(888,639),[179];(888,640),[50];(888,650),[214];(888,651),[52];(888,652),[212];(888,653),[223];(888,654),[221];(888,655),[229;230];(888,656),[232];(888,657),[239];(888,658),[242];(888,659),[245];(888,660),[254;255];(888,661),[249];(888,663),[247];(888,664),[253];(888,665),[251];(888,666),[258;259];(888,667),[257];(888,668),[261];(888,669),[266;267];(888,670),[265];(888,671),[263];(888,672),[270];(888,674),[272];(888,675),[276];(888,676),[274];(888,679),[291;292];(888,681),[300];(888,682),[306];(888,683),[302];(888,685),[304];(888,686),[308];(888,687),[309;310];(888,688),[312];(888,689),[318];(888,690),[314];(888,691),[316];(888,692),[320];(888,694),[330];(888,696),[334];(888,698),[338];(888,700),[348];(888,704),[366];(888,705),[368];(888,708),[741];(888,709),[150];(888,710),[157];(888,711),[163];(888,713),[215];(888,714),[224];(888,717),[293];(888,718),[322];(888,721),[359];(888,722),[390];(888,730),[74];(888,732),[682];(888,733),[691];(888,734),[704];(888,735),[742];(888,736),[747];(888,737),[894];(888,738),[151;152];(888,739),[159];(888,740),[165];(888,742),[217];(888,743),[226];(888,746),[295];(888,747),[323;324];(888,749),[360];(888,750),[391;392];(888,760),[684];(888,761),[692];(888,762),[76];(888,763),[705;706];(888,764),[743;744];(888,765),[748];(888,766),[895];(888,768),[154];(888,769),[161];(888,770),[167];(888,772),[219];(888,773),[228];(888,776),[297];(888,777),[326];(888,780),[394];(888,790),[686];(888,792),[708];(888,793),[746];(888,795),[78];(888,797),[12];(888,802),[528;529];(889,39),[666];(889,294),[1245];(889,728),[588];(889,757),[589];(890,350),[1330];(890,351),[1333];(890,619),[856];(890,620),[854];(892,72),[807];(892,355),[1342];(892,433),[1506];(892,513),[88];(892,524),[89];(892,634),[169];(892,737),[894];(892,766),[895];(893,453),[916];(894,12),[268];(894,32),[240];(894,34),[387];(894,67),[321];(894,97),[945];(894,109),[960];(894,146),[1011];(894,153),[1021];(894,155),[1023];(894,159),[1028];(894,170),[1055];(894,454),[886];(894,455),[405];(894,456),[370];(894,470),[71];(894,471),[378];(894,476),[389];(894,478),[409];(894,479),[413];(894,480),[417];(894,481),[79];(894,483),[419];(894,484),[423];(894,486),[425];(894,488),[429];(894,489),[433];(894,491),[435];(894,493),[437];(894,512),[512];(894,513),[88];(894,524),[90];(894,535),[92];(894,546),[94];(894,557),[96];(894,568),[98];(894,585),[728];(894,586),[726];(894,587),[724];(894,588),[730];(894,591),[105];(894,602),[109];(894,612),[820];(894,613),[110];(894,614),[822];(894,615),[824];(894,616),[826];(894,617),[828];(894,618),[830];(894,622),[861];(894,635),[171];(894,636),[173];(894,637),[174];(894,650),[214];(894,652),[212];(894,653),[223];(894,654),[221];(894,655),[229;230];(894,656),[232];(894,657),[239];(894,658),[241;242];(894,659),[245];(894,660),[254;255];(894,661),[249];(894,663),[247];(894,664),[253];(894,665),[251];(894,666),[258;259];(894,667),[257];(894,668),[261];(894,669),[266;267];(894,670),[265];(894,671),[263];(894,672),[270];(894,673),[65];(894,674),[272];(894,675),[276];(894,676),[274];(894,679),[291;292];(894,681),[300];(894,682),[306];(894,683),[302];(894,684),[67];(894,685),[304];(894,686),[308];(894,687),[309;310];(894,688),[312];(894,689),[318];(894,690),[314];(894,691),[316];(894,692),[320];(894,694),[330];(894,695),[69];(894,696),[334];(894,707),[101];(894,709),[150];(894,713),[215];(894,714),[224];(894,717),[293];(894,718),[322];(894,719),[57];(894,722),[390];(894,732),[682];(894,738),[152];(894,742),[217];(894,743),[226];(894,746),[295];(894,747),[324];(894,750),[392];(894,751),[59];(894,760),[684];(894,768),[154];(894,772),[219];(894,773),[228];(894,776),[297];(894,777),[326];(894,780),[394];(894,784),[61];(894,790),[686];(894,797),[12];(895,58),[277];(895,164),[1043];(895,715),[233];(895,744),[234];(896,376),[1388];(897,453),[915];(898,8),[581];(898,10),[561];(898,12),[268];(898,32),[240];(898,34),[387];(898,67),[321];(898,77),[632];(898,153),[1021];(898,155),[1023];(898,159),[1028];(898,170),[1055];(898,247),[1179];(898,259),[1193];(898,265),[1202];(898,276),[1215];(898,280),[1220];(898,281),[1223];(898,404),[1447];(898,454),[886];(898,455),[405];(898,456),[370];(898,459),[579;580];(898,460),[602];(898,461),[625];(898,471),[378];(898,476),[389];(898,478),[409];(898,479),[413];(898,480),[417];(898,483),[419];(898,484),[423];(898,486),[425];(898,488),[429];(898,489),[433];(898,491),[435];(898,493),[437];(898,512),[512];(898,526),[564];(898,527),[566];(898,528),[570];(898,530),[576];(898,531),[572];(898,532),[574];(898,533),[578];(898,534),[583];(898,536),[585];(898,537),[587];(898,538),[594];(898,539),[595];(898,541),[600];(898,542),[604];(898,544),[611];(898,545),[609];(898,547),[630;631];(898,549),[615];(898,550),[617];(898,551),[619];(898,552),[620];(898,553),[623];(898,554),[627];(898,556),[644];(898,585),[728];(898,586),[726];(898,587),[724];(898,588),[730];(898,612),[820];(898,614),[822];(898,615),[824];(898,616),[826];(898,617),[828];(898,618),[830];(898,622),[861];(898,650),[214];(898,652),[212];(898,653),[223];(898,654),[221];(898,655),[229;230];(898,656),[232];(898,657),[239];(898,658),[242];(898,659),[245];(898,660),[254;255];(898,661),[249];(898,663),[247];(898,664),[253];(898,665),[251];(898,666),[258;259];(898,667),[257];(898,668),[261];(898,669),[266;267];(898,670),[265];(898,671),[263];(898,672),[270];(898,674),[272];(898,675),[276];(898,676),[274];(898,679),[291;292];(898,681),[300];(898,682),[306];(898,683),[302];(898,685),[304];(898,686),[308];(898,687),[309;310];(898,688),[312];(898,689),[318];(898,690),[314];(898,691),[316];(898,692),[320];(898,694),[330];(898,696),[334];(898,709),[150];(898,713),[215];(898,714),[224];(898,717),[293];(898,718),[322];(898,722),[390];(898,726),[544];(898,727),[539];(898,732),[682];(898,738),[152];(898,742),[217];(898,743),[226];(898,746),[295];(898,747),[324];(898,750),[392];(898,755),[545;546];(898,756),[540];(898,760),[684];(898,768),[154];(898,772),[219];(898,773),[228];(898,776),[297];(898,777),[326];(898,780),[394];(898,785),[548];(898,790),[686];(898,797),[12];(899,393),[1424];(900,15),[481];(900,36),[486];(900,230),[1139];(900,237),[1150];(900,401),[1441];(900,402),[1444];(900,442),[1528];(900,725),[496];(900,754),[497];(900,800),[482;483];(901,92),[935];(901,673),[64];(902,12),[268];(902,32),[240];(902,33),[465];(902,34),[387];(902,43),[514;515];(902,64),[441];(902,67),[321];(902,79),[399];(902,153),[1021];(902,155),[1023];(902,159),[1028];(902,170),[1055];(902,204),[1106];(902,295),[1247];(902,390),[1416];(902,454),[886];(902,455),[405];(902,456),[370];(902,471),[378];(902,476),[389];(902,478),[409];(902,479),[413];(902,480),[417];(902,483),[419];(902,484),[423];(902,486),[425];(902,488),[429];(902,489),[433];(902,491),[435];(902,493),[437];(902,512),[512];(902,566),[673];(902,567),[671];(902,569),[675];(902,585),[728];(902,586),[726];(902,587),[724];(902,588),[730];(902,622),[861];(902,650),[214];(902,652),[212];(902,653),[223];(902,654),[221];(902,655),[229;230];(902,656),[232];(902,657),[239];(902,658),[242];(902,659),[245];(902,660),[254;255];(902,661),[249];(902,663),[247];(902,664),[253];(902,665),[251];(902,666),[258;259];(902,667),[257];(902,668),[261];(902,669),[266;267];(902,670),[265];(902,671),[263];(902,672),[270];(902,674),[272];(902,675),[276];(902,676),[274];(902,679),[291;292];(902,681),[300];(902,682),[306];(902,683),[302];(902,685),[304];(902,686),[308];(902,687),[309;310];(902,688),[312];(902,689),[318];(902,690),[314];(902,691),[316];(902,692),[320];(902,694),[330];(902,696),[334];(902,709),[150];(902,713),[215];(902,714),[224];(902,717),[293];(902,718),[322];(902,722),[390];(902,732),[682];(902,738),[152];(902,742),[217];(902,743),[226];(902,746),[295];(902,747),[324];(902,750),[391;392];(902,760),[684];(902,768),[154];(902,772),[219];(902,773),[228];(902,776),[297];(902,777),[326];(902,780),[394];(902,790),[686];(902,797),[12];(903,12),[268];(903,32),[240];(903,34),[387];(903,67),[321];(903,153),[1021];(903,155),[1023];(903,159),[1028];(903,170),[1055];(903,202),[1103];(903,351),[1333];(903,454),[886];(903,455),[405];(903,456),[370];(903,471),[378];(903,476),[388;389];(903,478),[409];(903,479),[413];(903,480),[417];(903,483),[419];(903,484),[423];(903,486),[425];(903,488),[429];(903,489),[433];(903,491),[435];(903,493),[437];(903,512),[512];(903,585),[728];(903,586),[726];(903,587),[724];(903,588),[730];(903,619),[856];(903,620),[855];(903,622),[861];(903,650),[214];(903,652),[212];(903,653),[223];(903,654),[221];(903,655),[229;230];(903,656),[232];(903,657),[239];(903,658),[242];(903,659),[245];(903,660),[254;255];(903,661),[249];(903,663),[247];(903,664),[253];(903,665),[251];(903,666),[258;259];(903,667),[257];(903,668),[261];(903,669),[266;267];(903,670),[265];(903,671),[263];(903,672),[270];(903,674),[272];(903,675),[276];(903,676),[274];(903,679),[291;292];(903,681),[300];(903,682),[306];(903,683),[302];(903,685),[304];(903,686),[308];(903,687),[309;310];(903,688),[312];(903,689),[318];(903,690),[314];(903,691),[316];(903,692),[320];(903,694),[330];(903,696),[334];(903,709),[150];(903,713),[215];(903,714),[224];(903,717),[293];(903,718),[322];(903,722),[390];(903,732),[682];(903,738),[152];(903,742),[217];(903,743),[226];(903,746),[295];(903,747),[324];(903,750),[392];(903,760),[684];(903,768),[154];(903,772),[219];(903,773),[228];(903,776),[297];(903,777),[326];(903,780),[394];(903,790),[686];(903,797),[12];(904,12),[268];(904,32),[240];(904,34),[387];(904,42),[516];(904,67),[321];(904,153),[1021];(904,155),[1023];(904,159),[1028];(904,170),[1055];(904,243),[1173];(904,403),[1445];(904,443),[1531];(904,454),[886];(904,455),[404;405];(904,456),[370];(904,471),[378];(904,476),[389];(904,478),[409];(904,479),[413];(904,480),[417];(904,483),[419];(904,484),[423];(904,486),[425];(904,488),[429];(904,489),[433];(904,491),[435];(904,493),[437];(904,512),[512];(904,514),[521];(904,585),[728];(904,586),[726];(904,587),[724];(904,588),[730];(904,622),[861];(904,650),[214];(904,652),[212];(904,653),[223];(904,654),[221];(904,655),[229;230];(904,656),[232];(904,657),[239];(904,658),[242];(904,659),[245];(904,660),[254;255];(904,661),[249];(904,663),[247];(904,664),[253];(904,665),[251];(904,666),[258;259];(904,667),[257];(904,668),[261];(904,669),[266;267];(904,670),[265];(904,671),[263];(904,672),[270];(904,674),[272];(904,675),[276];(904,676),[274];(904,679),[291;292];(904,681),[300];(904,682),[306];(904,683),[302];(904,685),[304];(904,686),[308];(904,687),[309;310];(904,688),[312];(904,689),[318];(904,690),[314];(904,691),[316];(904,692),[320];(904,694),[330];(904,696),[334];(904,709),[150];(904,713),[215];(904,714),[224];(904,717),[293];(904,718),[322];(904,722),[390];(904,732),[682];(904,738),[152];(904,742),[217];(904,743),[226];(904,746),[295];(904,747),[324];(904,750),[392];(904,760),[684];(904,768),[154];(904,772),[219];(904,773),[228];(904,776),[297];(904,777),[326];(904,780),[394];(904,790),[686];(904,797),[12];(905,349),[1326];(905,430),[1500];(905,619),[857];(905,621),[858];(906,12),[268];(906,32),[240];(906,34),[387];(906,67),[321];(906,153),[1021];(906,155),[1023];(906,159),[1028];(906,170),[1055];(906,296),[1249];(906,328),[1295];(906,425),[1489];(906,454),[886];(906,455),[405];(906,456),[370];(906,465),[115];(906,471),[378];(906,476),[389];(906,478),[409];(906,479),[413];(906,480),[417];(906,481),[80];(906,483),[419];(906,484),[423];(906,486),[425];(906,488),[429];(906,489),[433];(906,491),[435];(906,492),[73];(906,493),[437];(906,512),[512];(906,522),[556];(906,523),[558];(906,525),[560];(906,570),[679];(906,571),[681];(906,572),[688];(906,573),[690];(906,574),[697];(906,575),[700];(906,576),[702];(906,577),[712];(906,579),[30];(906,581),[716];(906,585),[728];(906,586),[726];(906,587),[724];(906,588),[730];(906,589),[733];(906,590),[735];(906,594),[757];(906,595),[764;765];(906,596),[759];(906,597),[761];(906,598),[763];(906,599),[767];(906,603),[777];(906,604),[779];(906,609),[794];(906,610),[796];(906,611),[798];(906,612),[820];(906,614),[822];(906,615),[824];(906,616),[826];(906,617),[828];(906,618),[830];(906,622),[861];(906,624),[113];(906,627),[117];(906,628),[137];(906,629),[48];(906,631),[146];(906,633),[156];(906,639),[179];(906,640),[50];(906,650),[214];(906,651),[52];(906,652),[212];(906,653),[223];(906,654),[221];(906,655),[229;230];(906,656),[232];(906,657),[239];(906,658),[242];(906,659),[245];(906,660),[254;255];(906,661),[249];(906,663),[247];(906,664),[253];(906,665),[251];(906,666),[258;259];(906,667),[257];(906,668),[261];(906,669),[266;267];(906,670),[265];(906,671),[263];(906,672),[270];(906,674),[272];(906,675),[276];(906,676),[274];(906,679),[291;292];(906,681),[300];(906,682),[306];(906,683),[302];(906,685),[304];(906,686),[308];(906,687),[309;310];(906,688),[312];(906,689),[318];(906,690),[314];(906,691),[316];(906,692),[320];(906,694),[330];(906,696),[334];(906,708),[741];(906,709),[150];(906,710),[157];(906,711),[163];(906,713),[215];(906,714),[224];(906,717),[293];(906,718),[322];(906,722),[390];(906,730),[74];(906,732),[682];(906,734),[704];(906,735),[742];(906,738),[152];(906,739),[159];(906,740),[165];(906,742),[217];(906,743),[226];(906,746),[295];(906,747),[324];(906,750),[392];(906,760),[684];(906,762),[76];(906,763),[706];(906,764),[743;744];(906,768),[154];(906,769),[161];(906,770),[167];(906,772),[219];(906,773),[228];(906,776),[297];(906,777),[326];(906,780),[394];(906,790),[686];(906,792),[708];(906,793),[746];(906,795),[78];(906,797),[12];(907,12),[268];(907,32),[240];(907,34),[387];(907,67),[321];(907,153),[1021];(907,155),[1023];(907,159),[1028];(907,170),[1055];(907,179),[1066];(907,454),[886];(907,455),[405];(907,456),[370];(907,471),[378];(907,476),[389];(907,478),[409];(907,479),[413];(907,480),[417];(907,483),[419];(907,484),[423];(907,486),[425];(907,488),[429];(907,489),[433];(907,491),[435];(907,493),[437];(907,512),[512];(907,585),[728];(907,586),[726];(907,587),[724];(907,588),[730];(907,622),[861];(907,650),[214];(907,652),[212];(907,653),[223];(907,654),[221];(907,655),[229;230];(907,656),[232];(907,657),[239];(907,658),[242];(907,659),[245];(907,660),[254;255];(907,661),[249];(907,663),[247];(907,664),[253];(907,665),[251];(907,666),[258;259];(907,667),[257];(907,668),[261];(907,669),[266;267];(907,670),[265];(907,671),[263];(907,672),[270];(907,674),[272];(907,675),[276];(907,676),[274];(907,679),[291;292];(907,681),[300];(907,682),[306];(907,683),[302];(907,685),[304];(907,686),[308];(907,687),[309;310];(907,688),[312];(907,689),[317;318];(907,690),[314];(907,691),[316];(907,692),[320];(907,694),[330];(907,696),[334];(907,709),[150];(907,713),[215];(907,714),[224];(907,717),[293];(907,718),[322];(907,722),[390];(907,732),[682];(907,738),[152];(907,742),[217];(907,743),[226];(907,746),[295];(907,747),[324];(907,750),[392];(907,760),[684];(907,768),[154];(907,772),[219];(907,773),[228];(907,776),[297];(907,777),[326];(907,780),[394];(907,790),[686];(907,797),[12];(908,41),[525];(908,58),[277];(908,164),[1045;1046;1047];(908,373),[1381];(908,374),[1383];(908,375),[1385];(908,515),[518];(908,516),[519];(908,715),[233];(908,744),[234];(909,12),[268];(909,32),[240];(909,34),[387];(909,67),[321];(909,153),[1021];(909,155),[1023];(909,159),[1028];(909,170),[1055];(909,180),[1067];(909,454),[886];(909,455),[405];(909,456),[370];(909,471),[378];(909,476),[389];(909,478),[409];(909,479),[413];(909,480),[417];(909,483),[419];(909,484),[423];(909,486),[425];(909,488),[429];(909,489),[433];(909,491),[435];(909,493),[437];(909,512),[512];(909,585),[728];(909,586),[726];(909,587),[724];(909,588),[730];(909,622),[861];(909,650),[214];(909,652),[212];(909,653),[223];(909,654),[221];(909,655),[229;230];(909,656),[232];(909,657),[239];(909,658),[242];(909,659),[245];(909,660),[254;255];(909,661),[249];(909,663),[247];(909,664),[253];(909,665),[251];(909,666),[258;259];(909,667),[257];(909,668),[261];(909,669),[266;267];(909,670),[265];(909,671),[263];(909,672),[270];(909,674),[272];(909,675),[276];(909,676),[274];(909,679),[291;292];(909,681),[300];(909,682),[306];(909,683),[302];(909,685),[304];(909,686),[308];(909,687),[309;310];(909,688),[312];(909,689),[318];(909,690),[314];(909,691),[316];(909,692),[319;320];(909,694),[330];(909,696),[334];(909,709),[150];(909,713),[215];(909,714),[224];(909,717),[293];(909,718),[322];(909,722),[390];(909,732),[682];(909,738),[152];(909,742),[217];(909,743),[226];(909,746),[295];(909,747),[324];(909,750),[392];(909,760),[684];(909,768),[154];(909,772),[219];(909,773),[228];(909,776),[297];(909,777),[326];(909,780),[394];(909,790),[686];(909,797),[12];(910,78),[503];(910,240),[1167];(910,463),[676];(910,509),[505];(911,10),[561];(911,12),[268];(911,32),[240];(911,34),[387];(911,67),[321];(911,127),[986];(911,153),[1021];(911,155),[1023];(911,159),[1028];(911,170),[1055];(911,181),[1069];(911,247),[1179];(911,258),[1191];(911,381),[1398];(911,382),[1400];(911,404),[1447];(911,436),[1515];(911,437),[1518];(911,446),[1539];(911,454),[886];(911,455),[405];(911,456),[370];(911,471),[378];(911,476),[389];(911,478),[409];(911,479),[413];(911,480),[417];(911,483),[419];(911,484),[423];(911,486),[425];(911,488),[429];(911,489),[433];(911,491),[435];(911,493),[437];(911,512),[512];(911,530),[575];(911,585),[728];(911,586),[726];(911,587),[724];(911,588),[730];(911,622),[861];(911,641),[185];(911,642),[190];(911,650),[214];(911,652),[212];(911,653),[223];(911,654),[221];(911,655),[229;230];(911,656),[232];(911,657),[239];(911,658),[242];(911,659),[245];(911,660),[254;255];(911,661),[249];(911,663),[247];(911,664),[253];(911,665),[251];(911,666),[258;259];(911,667),[257];(911,668),[261];(911,669),[266;267];(911,670),[265];(911,671),[263];(911,672),[270];(911,674),[272];(911,675),[276];(911,676),[274];(911,679),[291;292];(911,681),[300];(911,682),[306];(911,683),[302];(911,685),[304];(911,686),[308];(911,687),[309;310];(911,688),[312];(911,689),[318];(911,690),[314];(911,691),[316];(911,692),[320];(911,693),[328];(911,694),[330];(911,696),[334];(911,709),[150];(911,713),[215];(911,714),[224];(911,717),[293];(911,718),[322];(911,722),[390];(911,726),[544];(911,732),[682];(911,738),[152];(911,742),[217];(911,743),[226];(911,746),[295];(911,747),[323;324];(911,750),[392];(911,755),[545];(911,760),[684];(911,768),[154];(911,772),[219];(911,773),[228];(911,776),[297];(911,777),[326];(911,780),[394];(911,790),[686];(911,797),[12];(912,1),[37];(912,12),[268];(912,32),[240];(912,34),[387];(912,38),[144];(912,54),[129];(912,63),[28];(912,67),[321];(912,110),[961];(912,111),[962];(912,153),[1021];(912,155),[1023];(912,159),[1028];(912,170),[1055];(912,296),[1249];(912,302),[1256];(912,328),[1295];(912,425),[1489];(912,450),[899];(912,454),[886];(912,455),[405];(912,456),[370];(912,464),[32];(912,465),[114;115];(912,466),[142];(912,471),[378];(912,476),[389];(912,478),[409];(912,479),[413];(912,480),[417];(912,481),[80];(912,483),[419];(912,484),[423];(912,486),[425];(912,488),[429];(912,489),[433];(912,491),[435];(912,492),[73];(912,493),[437];(912,512),[512];(912,522),[556];(912,523),[558];(912,525),[560];(912,570),[679];(912,571),[681];(912,572),[688];(912,573),[689;690];(912,574),[697];(912,575),[700];(912,576),[702];(912,577),[712];(912,579),[31];(912,581),[716];(912,585),[728];(912,586),[726];(912,587),[724];(912,588),[730];(912,589),[733];(912,590),[735];(912,594),[757];(912,595),[764;765];(912,596),[759];(912,597),[761];(912,598),[763];(912,599),[767];(912,603),[777];(912,604),[779];(912,609),[794];(912,610),[796];(912,611),[798];(912,612),[820];(912,614),[822];(912,615),[824];(912,616),[826];(912,617),[828];(912,618),[830];(912,622),[861];(912,624),[113];(912,627),[117];(912,628),[137];(912,629),[48];(912,630),[141];(912,631),[146];(912,633),[156];(912,639),[179];(912,640),[50];(912,650),[214];(912,651),[52];(912,652),[212];(912,653),[223];(912,654),[221];(912,655),[229;230];(912,656),[232];(912,657),[239];(912,658),[242];(912,659),[245];(912,660),[254;255];(912,661),[249];(912,663),[247];(912,664),[253];(912,665),[251];(912,666),[258;259];(912,667),[257];(912,668),[261];(912,669),[266;267];(912,670),[265];(912,671),[263];(912,672),[270];(912,674),[272];(912,675),[276];(912,676),[274];(912,679),[291;292];(912,681),[300];(912,682),[306];(912,683),[302];(912,685),[304];(912,686),[308];(912,687),[309;310];(912,688),[312];(912,689),[318];(912,690),[314];(912,691),[316];(912,692),[320];(912,694),[330];(912,696),[334];(912,708),[741];(912,709),[150];(912,710),[157];(912,711),[163];(912,713),[215];(912,714),[224];(912,717),[293];(912,718),[322];(912,722),[390];(912,730),[74];(912,732),[682];(912,734),[704];(912,735),[742];(912,738),[152];(912,739),[159];(912,740),[165];(912,742),[217];(912,743),[226];(912,746),[295];(912,747),[324];(912,750),[392];(912,760),[684];(912,762),[76];(912,763),[706];(912,764),[743;744];(912,768),[154];(912,769),[161];(912,770),[167];(912,772),[219];(912,773),[228];(912,776),[297];(912,777),[326];(912,780),[394];(912,790),[686];(912,792),[708];(912,793),[746];(912,795),[78];(912,797),[12];(912,806),[29];(913,1),[44];(913,5),[893];(913,7),[196];(913,9),[183];(913,10),[562];(913,12),[268];(913,13),[283];(913,24),[703];(913,29),[739];(913,30),[738];(913,32),[240];(913,34),[387];(913,35),[9];(913,37),[6];(913,39),[666];(913,43),[514;515];(913,44),[717];(913,51),[373;374;375;376];(913,54),[132;133];(913,63),[28];(913,66),[364];(913,67),[321];(913,69),[55];(913,70),[53];(913,73),[875;876];(913,79),[398;399;400];(913,89),[930];(913,99),[948];(913,100),[949];(913,101),[950];(913,102),[951];(913,106),[957];(913,110),[961];(913,111),[962];(913,120),[975;976];(913,122),[980];(913,127),[987];(913,128),[989;990];(913,138),[1003];(913,153),[1021];(913,155),[1023];(913,159),[1028];(913,167),[1050;1051];(913,170),[1055];(913,181),[1068;1069;1070];(913,183),[1073];(913,185),[1075];(913,190),[1085];(913,193),[1091];(913,195),[1094];(913,204),[1105];(913,206),[1108];(913,208),[1110];(913,211),[1113];(913,215),[1118];(913,216),[1119];(913,223),[1128];(913,227),[1134];(913,247),[1179];(913,294),[1244];(913,295),[1248];(913,296),[1249];(913,302),[1256];(913,303),[1258];(913,308),[1266;1267];(913,309),[1268];(913,316),[1278];(913,319),[1284];(913,321),[1286];(913,328),[1295];(913,344),[1317];(913,355),[1341];(913,363),[1359;1360];(913,366),[1366];(913,367),[1367];(913,381),[1397;1398];(913,382),[1399;1400;1401];(913,384),[1405;1406];(913,385),[1407];(913,387),[1411];(913,392),[1420];(913,404),[1447];(913,419),[1479];(913,420),[1482;1483];(913,421),[1484];(913,422),[1486];(913,423),[1487];(913,424),[1488];(913,425),[1489;1491];(913,433),[1505];(913,434),[1508];(913,436),[1516];(913,437),[1517;1518];(913,438),[1519];(913,444),[1534;1535];(913,445),[1537];(913,446),[1540];(913,447),[14];(913,448),[21];(913,450),[899];(913,454),[886];(913,455),[405];(913,456),[370];(913,464),[32];(913,465),[114;115];(913,466),[142];(913,467),[181];(913,468),[350];(913,471),[378];(913,476),[389];(913,477),[407];(913,478),[409];(913,479),[413];(913,480),[416;417];(913,481),[80];(913,482),[415];(913,483),[419];(913,484),[422;423];(913,485),[421];(913,486),[425];(913,487),[427];(913,488),[429];(913,489),[432;433];(913,490),[431];(913,491),[435];(913,492),[73];(913,493),[437];(913,494),[444;445];(913,495),[446;447];(913,503),[82;83];(913,512),[512];(913,513),[87];(913,517),[532];(913,522),[556];(913,523),[558];(913,525),[560];(913,543),[606];(913,555),[629];(913,564),[660];(913,566),[673];(913,567),[671];(913,569),[675];(913,570),[679];(913,571),[681];(913,572),[688];(913,573),[689;690];(913,574),[697];(913,575),[700];(913,576),[702];(913,577),[712];(913,579),[31];(913,580),[102;103];(913,581),[716];(913,583),[721];(913,585),[727;728];(913,586),[726];(913,587),[724];(913,588),[730];(913,589),[733];(913,590),[735];(913,592),[753];(913,593),[755];(913,594),[757];(913,595),[764;765];(913,596),[759];(913,597),[761];(913,598),[763];(913,599),[767];(913,600),[769];(913,603),[777];(913,604),[779];(913,609),[794];(913,610),[796];(913,611),[798];(913,612),[820];(913,614),[822];(913,615),[824];(913,616),[826];(913,617),[828];(913,618),[830];(913,622),[861];(913,624),[113];(913,627),[117];(913,628),[137];(913,629),[48];(913,630),[141];(913,631),[146];(913,633),[156];(913,635),[171];(913,636),[173];(913,637),[175];(913,639),[179];(913,640),[50];(913,641),[185];(913,642),[190;191];(913,643),[187];(913,644),[189];(913,645),[193];(913,646),[194;195];(913,647),[198];(913,648),[200];(913,650),[214];(913,651),[52];(913,652),[212];(913,653),[223];(913,654),[221];(913,655),[229;230];(913,656),[232];(913,657),[239];(913,658),[242];(913,659),[245];(913,660),[254;255];(913,661),[249];(913,663),[247];(913,664),[253];(913,665),[251];(913,666),[258;259];(913,667),[257];(913,668),[261];(913,669),[266;267];(913,670),[265];(913,671),[263];(913,672),[270];(913,674),[272];(913,675),[276];(913,676),[274];(913,679),[291;292];(913,681),[300];(913,682),[306];(913,683),[302];(913,685),[304];(913,686),[308];(913,687),[309;310];(913,688),[312];(913,689),[318];(913,690),[314];(913,691),[316];(913,692),[320];(913,693),[327;328];(913,694),[330];(913,696),[333;334];(913,697),[332];(913,698),[338];(913,700),[348];(913,701),[352];(913,704),[366];(913,705),[368];(913,706),[8];(913,708),[741];(913,709),[150];(913,710),[157];(913,711),[163];(913,712),[206];(913,713),[215];(913,714),[224];(913,717),[293];(913,718),[322];(913,721),[359];(913,722),[390];(913,723),[466];(913,726),[544];(913,728),[588];(913,730),[74];(913,732),[682];(913,733),[691];(913,734),[704];(913,735),[742];(913,736),[747];(913,737),[894];(913,738),[151;152];(913,739),[159];(913,740),[164;165];(913,741),[207];(913,742),[216;217];(913,743),[226];(913,746),[295];(913,747),[323;324];(913,749),[360];(913,750),[391;392];(913,752),[467];(913,755),[545];(913,757),[589];(913,760),[684];(913,761),[692];(913,762),[75;76];(913,763),[705;706];(913,764),[743;744];(913,765),[748];(913,766),[895];(913,767),[10];(913,768),[154];(913,769),[161];(913,770),[167];(913,772),[219];(913,773),[228];(913,776),[297];(913,777),[326];(913,780),[394];(913,790),[686];(913,792),[708];(913,793),[746];(913,795),[78];(913,797),[12];(913,798),[84;85];(913,802),[528;529];(913,806),[29];(914,453),[914];(915,12),[268];(915,32),[240];(915,34),[387];(915,67),[321];(915,153),[1021];(915,155),[1023];(915,159),[1028];(915,170),[1055];(915,454),[881;886];(915,455),[405];(915,456),[370];(915,471),[378];(915,474),[384];(915,475),[386];(915,476),[389];(915,478),[409];(915,479),[413];(915,480),[417];(915,483),[419];(915,484),[423];(915,486),[425];(915,488),[429];(915,489),[433];(915,491),[435];(915,493),[437];(915,512),[512];(915,585),[728];(915,586),[726];(915,587),[724];(915,588),[730];(915,622),[861];(915,625),[890];(915,650),[214];(915,652),[212];(915,653),[223];(915,654),[221];(915,655),[229;230];(915,656),[232];(915,657),[239];(915,658),[242];(915,659),[245];(915,660),[254;255];(915,661),[249];(915,663),[247];(915,664),[253];(915,665),[251];(915,666),[258;259];(915,667),[257];(915,668),[261];(915,669),[266;267];(915,670),[265];(915,671),[263];(915,672),[270];(915,674),[272];(915,675),[276];(915,676),[274];(915,679),[291;292];(915,681),[300];(915,682),[306];(915,683),[302];(915,685),[304];(915,686),[308];(915,687),[309;310];(915,688),[312];(915,689),[318];(915,690),[314];(915,691),[316];(915,692),[320];(915,694),[330];(915,696),[334];(915,709),[150];(915,713),[215];(915,714),[224];(915,717),[293];(915,718),[322];(915,722),[390];(915,732),[682];(915,738),[152];(915,742),[217];(915,743),[226];(915,746),[295];(915,747),[324];(915,750),[392];(915,760),[684];(915,768),[154];(915,772),[219];(915,773),[228];(915,776),[297];(915,777),[326];(915,780),[394];(915,790),[686];(915,797),[12];(916,78),[503];(916,240),[1169];(916,463),[676];(916,509),[505];(917,232),[1144];(918,12),[268];(918,32),[240];(918,34),[387];(918,42),[516];(918,67),[321];(918,153),[1021];(918,155),[1023];(918,159),[1028];(918,170),[1055];(918,243),[1174];(918,454),[886];(918,455),[404;405];(918,456),[370];(918,471),[378];(918,476),[389];(918,478),[409];(918,479),[413];(918,480),[417];(918,483),[419];(918,484),[423];(918,486),[425];(918,488),[429];(918,489),[433];(918,491),[435];(918,493),[437];(918,512),[512];(918,514),[521];(918,585),[728];(918,586),[726];(918,587),[724];(918,588),[730];(918,622),[861];(918,650),[214];(918,652),[212];(918,653),[223];(918,654),[221];(918,655),[229;230];(918,656),[232];(918,657),[239];(918,658),[242];(918,659),[245];(918,660),[254;255];(918,661),[249];(918,663),[247];(918,664),[253];(918,665),[251];(918,666),[258;259];(918,667),[257];(918,668),[261];(918,669),[266;267];(918,670),[265];(918,671),[263];(918,672),[270];(918,674),[272];(918,675),[276];(918,676),[274];(918,679),[291;292];(918,681),[300];(918,682),[306];(918,683),[302];(918,685),[304];(918,686),[308];(918,687),[309;310];(918,688),[312];(918,689),[318];(918,690),[314];(918,691),[316];(918,692),[320];(918,694),[330];(918,696),[334];(918,709),[150];(918,713),[215];(918,714),[224];(918,717),[293];(918,718),[322];(918,722),[390];(918,732),[682];(918,738),[152];(918,742),[217];(918,743),[226];(918,746),[295];(918,747),[324];(918,750),[392];(918,760),[684];(918,768),[154];(918,772),[219];(918,773),[228];(918,776),[297];(918,777),[326];(918,780),[394];(918,790),[686];(918,797),[12];(919,1),[34];(919,12),[268];(919,32),[240];(919,34),[387];(919,40),[667];(919,54),[120];(919,63),[28];(919,67),[321];(919,110),[961];(919,111),[962];(919,153),[1021];(919,155),[1023];(919,159),[1028];(919,170),[1055];(919,296),[1249];(919,302),[1256];(919,328),[1295];(919,425),[1489];(919,450),[899];(919,454),[886];(919,455),[405];(919,456),[370];(919,464),[32];(919,465),[114;115];(919,466),[142];(919,471),[378];(919,476),[389];(919,478),[409];(919,479),[413];(919,480),[417];(919,481),[80];(919,483),[419];(919,484),[423];(919,486),[425];(919,488),[429];(919,489),[433];(919,491),[435];(919,492),[73];(919,493),[437];(919,512),[512];(919,522),[556];(919,523),[558];(919,525),[560];(919,565),[669];(919,570),[679];(919,571),[681];(919,572),[688];(919,573),[689;690];(919,574),[697];(919,575),[700];(919,576),[702];(919,577),[712];(919,579),[31];(919,581),[716];(919,585),[728];(919,586),[726];(919,587),[724];(919,588),[730];(919,589),[733];(919,590),[735];(919,594),[757];(919,595),[764;765];(919,596),[759];(919,597),[761];(919,598),[763];(919,599),[767];(919,603),[777];(919,604),[779];(919,609),[794];(919,610),[796];(919,611),[798];(919,612),[820];(919,614),[822];(919,615),[824];(919,616),[826];(919,617),[828];(919,618),[830];(919,622),[861];(919,624),[113];(919,627),[117];(919,628),[137];(919,629),[48];(919,630),[141];(919,631),[146];(919,633),[156];(919,639),[179];(919,640),[50];(919,650),[214];(919,651),[52];(919,652),[212];(919,653),[223];(919,654),[221];(919,655),[229;230];(919,656),[232];(919,657),[239];(919,658),[242];(919,659),[245];(919,660),[254;255];(919,661),[249];(919,663),[247];(919,664),[253];(919,665),[251];(919,666),[258;259];(919,667),[257];(919,668),[261];(919,669),[266;267];(919,670),[265];(919,671),[263];(919,672),[270];(919,674),[272];(919,675),[276];(919,676),[274];(919,678),[287];(919,679),[291;292];(919,681),[300];(919,682),[306];(919,683),[302];(919,685),[304];(919,686),[308];(919,687),[309;310];(919,688),[312];(919,689),[318];(919,690),[314];(919,691),[316];(919,692),[320];(919,694),[330];(919,696),[334];(919,708),[741];(919,709),[150];(919,710),[157];(919,711),[163];(919,712),[206];(919,713),[215];(919,714),[224];(919,717),[293];(919,718),[322];(919,722),[390];(919,730),[74];(919,732),[682];(919,734),[704];(919,735),[742];(919,738),[152];(919,739),[159];(919,740),[165];(919,741),[208];(919,742),[217];(919,743),[226];(919,746),[295];(919,747),[324];(919,750),[392];(919,760),[684];(919,762),[76];(919,763),[706];(919,764),[743;744];(919,768),[154];(919,769),[161];(919,770),[167];(919,771),[210];(919,772),[219];(919,773),[228];(919,776),[297];(919,777),[326];(919,780),[394];(919,790),[686];(919,792),[708];(919,793),[746];(919,795),[78];(919,797),[12];(919,806),[29];(920,72),[810];(920,355),[1342];(920,433),[1506];(920,513),[88];(920,524),[89];(920,634),[169];(920,737),[894];(920,766),[895];(921,12),[268];(921,32),[240];(921,34),[387];(921,67),[321];(921,153),[1021];(921,155),[1023];(921,159),[1028];(921,170),[1055;1056];(921,378),[1391];(921,454),[886];(921,455),[405];(921,456),[370];(921,471),[378];(921,476),[389];(921,478),[409];(921,479),[413];(921,480),[417];(921,483),[419];(921,484),[423];(921,486),[425];(921,488),[429];(921,489),[433];(921,491),[435];(921,493),[437];(921,512),[512];(921,585),[728];(921,586),[726];(921,587),[724];(921,588),[730];(921,622),[861];(921,650),[214];(921,652),[212];(921,653),[223];(921,654),[221];(921,655),[229;230];(921,656),[232];(921,657),[239];(921,658),[242];(921,659),[245];(921,660),[254;255];(921,661),[249];(921,663),[247];(921,664),[253];(921,665),[251];(921,666),[258;259];(921,667),[257];(921,668),[261];(921,669),[266;267];(921,670),[265];(921,671),[263];(921,672),[270];(921,674),[272];(921,675),[276];(921,676),[274];(921,679),[291;292];(921,681),[300];(921,682),[306];(921,683),[302];(921,685),[304];(921,686),[308];(921,687),[309;310];(921,688),[312];(921,689),[318];(921,690),[314];(921,691),[316];(921,692),[320];(921,694),[330];(921,696),[334];(921,709),[150];(921,713),[215];(921,714),[224];(921,717),[293];(921,718),[322];(921,722),[390];(921,732),[682];(921,738),[152];(921,742),[217];(921,743),[226];(921,746),[295];(921,747),[324];(921,750),[392];(921,760),[684];(921,768),[154];(921,772),[219];(921,773),[228];(921,776),[297];(921,777),[326];(921,780),[394];(921,790),[686];(921,797),[12];(922,12),[268];(922,32),[240];(922,34),[387];(922,67),[321];(922,153),[1021];(922,155),[1023];(922,159),[1028];(922,170),[1055];(922,176),[1063];(922,352),[1334];(922,454),[886];(922,455),[405];(922,456),[370];(922,471),[378];(922,476),[389];(922,478),[409];(922,479),[413];(922,480),[417];(922,483),[419];(922,484),[423];(922,486),[425];(922,488),[429];(922,489),[433];(922,491),[435];(922,493),[437];(922,512),[512];(922,566),[673];(922,567),[671];(922,569),[674];(922,577),[712];(922,581),[716];(922,585),[728];(922,586),[726];(922,587),[724];(922,588),[730];(922,622),[860;861];(922,650),[214];(922,652),[212];(922,653),[223];(922,654),[221];(922,655),[229;230];(922,656),[232];(922,657),[239];(922,658),[242];(922,659),[245];(922,660),[254;255];(922,661),[249];(922,663),[247];(922,664),[253];(922,665),[251];(922,666),[258;259];(922,667),[257];(922,668),[261];(922,669),[266;267];(922,670),[265];(922,671),[263];(922,672),[270];(922,674),[272];(922,675),[276];(922,676),[274];(922,679),[291;292];(922,681),[300];(922,682),[306];(922,683),[302];(922,685),[304];(922,686),[307;308];(922,687),[309;310];(922,688),[312];(922,689),[318];(922,690),[314];(922,691),[316];(922,692),[320];(922,694),[330];(922,696),[334];(922,709),[150];(922,713),[215];(922,714),[224];(922,717),[293];(922,718),[322];(922,722),[390];(922,732),[682];(922,734),[704];(922,738),[152];(922,742),[217];(922,743),[226];(922,746),[295];(922,747),[324];(922,750),[392];(922,760),[684];(922,763),[706];(922,768),[154];(922,772),[219];(922,773),[228];(922,776),[297];(922,777),[326];(922,780),[394];(922,790),[686];(922,792),[708];(922,797),[12];(923,357),[1347];(924,12),[268];(924,32),[240];(924,34),[387];(924,67),[321];(924,153),[1021];(924,155),[1023];(924,159),[1028];(924,170),[1055];(924,454),[884;886];(924,455),[405];(924,456),[370];(924,471),[378];(924,476),[389];(924,478),[409];(924,479),[413];(924,480),[417];(924,483),[419];(924,484),[423];(924,486),[425];(924,488),[429];(924,489),[433];(924,491),[435];(924,493),[437];(924,512),[512];(924,585),[728];(924,586),[726];(924,587),[724];(924,588),[730];(924,622),[861];(924,650),[214];(924,652),[212];(924,653),[223];(924,654),[221];(924,655),[229;230];(924,656),[232];(924,657),[239];(924,658),[242];(924,659),[245];(924,660),[254;255];(924,661),[249];(924,663),[247];(924,664),[253];(924,665),[251];(924,666),[258;259];(924,667),[257];(924,668),[261];(924,669),[266;267];(924,670),[265];(924,671),[263];(924,672),[270];(924,674),[272];(924,675),[276];(924,676),[274];(924,679),[291;292];(924,681),[300];(924,682),[306];(924,683),[302];(924,685),[304];(924,686),[308];(924,687),[309;310];(924,688),[312];(924,689),[318];(924,690),[314];(924,691),[316];(924,692),[320];(924,694),[330];(924,696),[334];(924,709),[150];(924,713),[215];(924,714),[224];(924,717),[293];(924,718),[322];(924,722),[390];(924,732),[682];(924,738),[152];(924,742),[217];(924,743),[226];(924,746),[295];(924,747),[324];(924,750),[392];(924,760),[684];(924,768),[154];(924,772),[219];(924,773),[228];(924,776),[297];(924,777),[326];(924,780),[394];(924,790),[686];(924,797),[12];(925,15),[481];(925,84),[495];(925,230),[1137];(925,800),[482;483];(926,12),[268];(926,24),[703];(926,32),[240];(926,34),[387];(926,43),[514;515];(926,51),[373;374;375;376];(926,66),[364];(926,67),[321];(926,73),[864];(926,79),[399];(926,153),[1021];(926,155),[1023];(926,159),[1028];(926,170),[1055];(926,181),[1069];(926,193),[1091];(926,195),[1094];(926,204),[1105];(926,303),[1258];(926,308),[1266];(926,309),[1268];(926,321),[1286];(926,328),[1295];(926,355),[1341];(926,381),[1397];(926,382),[1400];(926,385),[1407];(926,387),[1411];(926,419),[1479];(926,420),[1482];(926,421),[1484];(926,424),[1488];(926,425),[1491];(926,433),[1505];(926,437),[1517];(926,444),[1534];(926,454),[886];(926,455),[405];(926,456),[370];(926,471),[378];(926,476),[389];(926,478),[409];(926,479),[413];(926,480),[417];(926,483),[419];(926,484),[423];(926,486),[425];(926,488),[429];(926,489),[433];(926,491),[435];(926,493),[437];(926,512),[512];(926,517),[532];(926,543),[606];(926,555),[629];(926,564),[660];(926,577),[712];(926,585),[728];(926,586),[726];(926,587),[724];(926,588),[730];(926,594),[757];(926,595),[764];(926,596),[759];(926,600),[769];(926,622),[861];(926,635),[171];(926,636),[173];(926,637),[175];(926,650),[214];(926,652),[212];(926,653),[223];(926,654),[221];(926,655),[229;230];(926,656),[232];(926,657),[239];(926,658),[242];(926,659),[245];(926,660),[254;255];(926,661),[249];(926,663),[247];(926,664),[253];(926,665),[251];(926,666),[258;259];(926,667),[257];(926,668),[261];(926,669),[266;267];(926,670),[265];(926,671),[263];(926,672),[270];(926,674),[272];(926,675),[276];(926,676),[274];(926,679),[291;292];(926,681),[300];(926,682),[306];(926,683),[302];(926,685),[304];(926,686),[308];(926,687),[309;310];(926,688),[312];(926,689),[318];(926,690),[314];(926,691),[316];(926,692),[320];(926,694),[330];(926,696),[334];(926,698),[338];(926,700),[348];(926,704),[366];(926,705),[368];(926,708),[741];(926,709),[150];(926,713),[215];(926,714),[224];(926,717),[293];(926,718),[322];(926,721),[359];(926,722),[390];(926,732),[682];(926,733),[691];(926,734),[704];(926,735),[742];(926,736),[747];(926,737),[894];(926,738),[151;152];(926,742),[217];(926,743),[226];(926,746),[295];(926,747),[323;324];(926,749),[360];(926,750),[391;392];(926,760),[684];(926,761),[692];(926,763),[705];(926,764),[743];(926,765),[748];(926,766),[895];(926,768),[154];(926,772),[219];(926,773),[228];(926,776),[297];(926,777),[326];(926,780),[394];(926,790),[686];(926,797),[12];(926,802),[528;529];(927,12),[268];(927,32),[240];(927,34),[387];(927,42),[516];(927,67),[321];(927,153),[1021];(927,155),[1023];(927,159),[1028];(927,170),[1055];(927,454),[886];(927,455),[404;405];(927,456),[370];(927,471),[378];(927,476),[389];(927,478),[409];(927,479),[413];(927,480),[417];(927,483),[419];(927,484),[423];(927,486),[425];(927,488),[429];(927,489),[433];(927,491),[435];(927,493),[437];(927,512),[512];(927,514),[522];(927,515),[518];(927,516),[520];(927,585),[728];(927,586),[726];(927,587),[724];(927,588),[730];(927,622),[861];(927,650),[214];(927,652),[212];(927,653),[223];(927,654),[221];(927,655),[229;230];(927,656),[232];(927,657),[239];(927,658),[242];(927,659),[245];(927,660),[254;255];(927,661),[249];(927,663),[247];(927,664),[253];(927,665),[251];(927,666),[258;259];(927,667),[257];(927,668),[261];(927,669),[266;267];(927,670),[265];(927,671),[263];(927,672),[270];(927,674),[272];(927,675),[276];(927,676),[274];(927,679),[291;292];(927,681),[300];(927,682),[306];(927,683),[302];(927,685),[304];(927,686),[308];(927,687),[309;310];(927,688),[312];(927,689),[318];(927,690),[314];(927,691),[316];(927,692),[320];(927,694),[330];(927,696),[334];(927,709),[150];(927,713),[215];(927,714),[224];(927,717),[293];(927,718),[322];(927,722),[390];(927,732),[682];(927,738),[152];(927,742),[217];(927,743),[226];(927,746),[295];(927,747),[324];(927,750),[392];(927,760),[684];(927,768),[154];(927,772),[219];(927,773),[228];(927,776),[297];(927,777),[326];(927,780),[394];(927,790),[686];(927,797),[12];(928,58),[277];(928,164),[1037];(928,715),[233];(928,744),[234];(929,58),[277];(929,164),[1036];(929,715),[233];(929,744),[234];(931,93),[938];(931,673),[65];(931,684),[66];(933,350),[1329];(933,351),[1333];(933,619),[856];(933,620),[854];(935,12),[268];(935,32),[240];(935,34),[387];(935,42),[516];(935,67),[321];(935,153),[1021];(935,155),[1023];(935,159),[1028];(935,170),[1055];(935,243),[1173];(935,403),[1445];(935,443),[1533];(935,454),[886];(935,455),[404;405];(935,456),[370];(935,471),[378];(935,476),[389];(935,478),[409];(935,479),[413];(935,480),[417];(935,483),[419];(935,484),[423];(935,486),[425];(935,488),[429];(935,489),[433];(935,491),[435];(935,493),[437];(935,512),[512];(935,514),[521];(935,585),[728];(935,586),[726];(935,587),[724];(935,588),[730];(935,622),[861];(935,650),[214];(935,652),[212];(935,653),[223];(935,654),[221];(935,655),[229;230];(935,656),[232];(935,657),[239];(935,658),[242];(935,659),[245];(935,660),[254;255];(935,661),[249];(935,663),[247];(935,664),[253];(935,665),[251];(935,666),[258;259];(935,667),[257];(935,668),[261];(935,669),[266;267];(935,670),[265];(935,671),[263];(935,672),[270];(935,674),[272];(935,675),[276];(935,676),[274];(935,679),[291;292];(935,681),[300];(935,682),[306];(935,683),[302];(935,685),[304];(935,686),[308];(935,687),[309;310];(935,688),[312];(935,689),[318];(935,690),[314];(935,691),[316];(935,692),[320];(935,694),[330];(935,696),[334];(935,709),[150];(935,713),[215];(935,714),[224];(935,717),[293];(935,718),[322];(935,722),[390];(935,732),[682];(935,738),[152];(935,742),[217];(935,743),[226];(935,746),[295];(935,747),[324];(935,750),[392];(935,760),[684];(935,768),[154];(935,772),[219];(935,773),[228];(935,776),[297];(935,777),[326];(935,780),[394];(935,790),[686];(935,797),[12];(936,12),[268];(936,32),[240];(936,34),[387];(936,67),[321];(936,153),[1021];(936,155),[1023];(936,159),[1028];(936,170),[1055];(936,454),[880;886];(936,455),[405];(936,456),[370];(936,471),[378];(936,476),[389];(936,478),[409];(936,479),[413];(936,480),[417];(936,483),[419];(936,484),[423];(936,486),[425];(936,488),[429];(936,489),[433];(936,491),[435];(936,493),[437];(936,512),[512];(936,585),[728];(936,586),[726];(936,587),[724];(936,588),[730];(936,622),[861];(936,626),[892];(936,650),[214];(936,652),[212];(936,653),[223];(936,654),[221];(936,655),[229;230];(936,656),[232];(936,657),[239];(936,658),[242];(936,659),[245];(936,660),[254;255];(936,661),[249];(936,663),[247];(936,664),[253];(936,665),[251];(936,666),[258;259];(936,667),[257];(936,668),[261];(936,669),[266;267];(936,670),[265];(936,671),[263];(936,672),[270];(936,674),[272];(936,675),[276];(936,676),[274];(936,679),[291;292];(936,681),[300];(936,682),[306];(936,683),[302];(936,685),[304];(936,686),[308];(936,687),[309;310];(936,688),[312];(936,689),[318];(936,690),[314];(936,691),[316];(936,692),[320];(936,694),[330];(936,696),[334];(936,709),[150];(936,713),[215];(936,714),[224];(936,717),[293];(936,718),[322];(936,722),[390];(936,732),[682];(936,738),[152];(936,742),[217];(936,743),[226];(936,746),[295];(936,747),[324];(936,750),[392];(936,760),[684];(936,768),[154];(936,772),[219];(936,773),[228];(936,776),[297];(936,777),[326];(936,780),[394];(936,790),[686];(936,797),[12];(937,635),[170];(938,118),[973];(938,362),[1358];(938,710),[157];(938,739),[158];(939,428),[1497];(939,429),[1499];(940,41),[526];(940,58),[277];(940,164),[1045];(940,375),[1387];(940,515),[518];(940,516),[519];(940,715),[233];(940,744),[234];(941,12),[268];(941,24),[703];(941,32),[240];(941,34),[387];(941,43),[514;515];(941,51),[373;374;375;376];(941,65),[336];(941,66),[364];(941,67),[321];(941,73),[878];(941,79),[399];(941,153),[1021];(941,155),[1023];(941,159),[1028];(941,170),[1055];(941,181),[1069];(941,193),[1091];(941,195),[1094];(941,204),[1105];(941,303),[1258];(941,308),[1266];(941,309),[1268];(941,321),[1286];(941,328),[1295];(941,355),[1341];(941,381),[1397];(941,382),[1400];(941,385),[1407];(941,387),[1411];(941,419),[1479];(941,420),[1482];(941,421),[1484];(941,424),[1488];(941,425),[1491];(941,433),[1505];(941,437),[1517];(941,444),[1534];(941,454),[886];(941,455),[405];(941,456),[370];(941,471),[378];(941,476),[389];(941,478),[409];(941,479),[413];(941,480),[417];(941,483),[419];(941,484),[423];(941,486),[425];(941,488),[429];(941,489),[433];(941,491),[435];(941,493),[437];(941,512),[512];(941,517),[532];(941,543),[606];(941,555),[629];(941,564),[660];(941,577),[712];(941,585),[728];(941,586),[726];(941,587),[724];(941,588),[730];(941,594),[757];(941,595),[764];(941,596),[759];(941,600),[769];(941,622),[861];(941,635),[171];(941,636),[173];(941,637),[175];(941,650),[214];(941,652),[212];(941,653),[223];(941,654),[221];(941,655),[229;230];(941,656),[232];(941,657),[239];(941,658),[242];(941,659),[245];(941,660),[254;255];(941,661),[249];(941,663),[247];(941,664),[253];(941,665),[251];(941,666),[258;259];(941,667),[257];(941,668),[261];(941,669),[266;267];(941,670),[265];(941,671),[263];(941,672),[270];(941,674),[272];(941,675),[276];(941,676),[274];(941,679),[291;292];(941,681),[300];(941,682),[306];(941,683),[302];(941,685),[304];(941,686),[308];(941,687),[309;310];(941,688),[312];(941,689),[318];(941,690),[314];(941,691),[316];(941,692),[320];(941,694),[330];(941,696),[334];(941,698),[338];(941,700),[348];(941,704),[366];(941,705),[368];(941,708),[741];(941,709),[150];(941,713),[215];(941,714),[224];(941,717),[293];(941,718),[322];(941,721),[359];(941,722),[390];(941,732),[682];(941,733),[691];(941,734),[704];(941,735),[742];(941,736),[747];(941,737),[894];(941,738),[151;152];(941,742),[217];(941,743),[226];(941,746),[295];(941,747),[323;324];(941,749),[360];(941,750),[391;392];(941,760),[684];(941,761),[692];(941,763),[705];(941,764),[743];(941,765),[748];(941,766),[895];(941,768),[154];(941,772),[219];(941,773),[228];(941,776),[297];(941,777),[326];(941,780),[394];(941,790),[686];(941,797),[12];(941,802),[528;529];(943,142),[1007];(943,342),[1313];(943,368),[1370];(943,369),[1373];(943,435),[1511];(943,714),[224];(943,743),[225];(944,58),[277];(944,164),[1042];(944,715),[233];(944,744),[234];(946,58),[277];(946,164),[1034];(946,715),[233];(946,744),[234];(948,41),[524];(948,58),[277];(948,164),[1045;1046];(948,374),[1382];(948,375),[1386];(948,515),[518];(948,516),[519];(948,715),[233];(948,744),[234];(949,142),[1007];(949,368),[1369];(949,369),[1373];(949,435),[1510];(949,714),[224];(949,743),[225];(950,72),[808];(950,355),[1342];(950,433),[1506];(950,513),[88];(950,524),[89];(950,634),[169];(950,737),[894];(950,766),[895];(951,72),[802];(951,355),[1342];(951,433),[1506];(951,513),[88];(951,524),[89];(951,634),[169];(951,737),[894];(951,766),[895];(952,235),[1148];(952,236),[1149];(952,397),[1433];(952,399),[1437];(952,801),[493;494];(953,350),[1332];(953,351),[1333];(953,619),[856];(953,620),[854];(954,23),[665];(955,453),[913];(956,453),[912];(957,78),[503];(957,463),[676];(957,509),[504];(958,78),[503];(958,240),[1166];(958,463),[676];(958,509),[505];(959,261),[1196];(959,273),[1210];(959,286),[1229];(959,536),[584];(959,549),[614];(959,558),[645];(960,7),[196];(960,12),[268];(960,24),[703];(960,32),[240];(960,34),[387];(960,43),[514;515];(960,51),[373;374;375;376];(960,66),[364];(960,67),[321];(960,73),[873];(960,79),[399];(960,127),[987];(960,128),[990];(960,129),[991];(960,153),[1021];(960,155),[1023];(960,159),[1028];(960,170),[1055];(960,181),[1069];(960,193),[1091];(960,195),[1094];(960,204),[1105];(960,253),[1185];(960,257),[1190];(960,268),[1205];(960,269),[1206];(960,270),[1207];(960,278),[1217];(960,279),[1218];(960,291),[1237];(960,292),[1238];(960,303),[1258];(960,308),[1266];(960,309),[1268];(960,321),[1286];(960,328),[1295];(960,353),[1338];(960,355),[1341];(960,381),[1397];(960,382),[1400];(960,385),[1407];(960,387),[1411];(960,419),[1479];(960,420),[1482];(960,421),[1484];(960,424),[1488];(960,425),[1491];(960,433),[1505];(960,437),[1517];(960,444),[1534];(960,454),[880;881;886];(960,455),[405];(960,456),[370];(960,460),[602];(960,461),[625];(960,462),[656];(960,471),[378];(960,472),[379];(960,473),[381];(960,476),[389];(960,478),[409];(960,479),[413];(960,480),[417];(960,483),[419];(960,484),[423];(960,486),[425];(960,488),[429];(960,489),[433];(960,491),[435];(960,493),[437];(960,512),[512];(960,517),[532];(960,526),[564];(960,527),[565];(960,531),[572];(960,532),[573];(960,541),[600];(960,542),[603];(960,543),[605;606];(960,545),[608];(960,553),[623];(960,554),[626];(960,555),[628;629];(960,562),[654];(960,563),[657];(960,564),[659;660];(960,577),[712];(960,585),[728];(960,586),[726];(960,587),[724];(960,588),[730];(960,594),[757];(960,595),[764];(960,596),[759];(960,600),[769];(960,612),[820];(960,614),[822];(960,615),[824];(960,616),[826];(960,617),[828];(960,618),[830];(960,622),[861];(960,623),[887];(960,625),[889];(960,626),[891];(960,635),[171];(960,636),[173];(960,637),[175];(960,641),[185];(960,642),[190;191];(960,643),[187];(960,644),[189];(960,645),[193];(960,646),[194];(960,647),[197];(960,650),[214];(960,652),[212];(960,653),[223];(960,654),[221];(960,655),[229;230];(960,656),[232];(960,657),[239];(960,658),[242];(960,659),[245];(960,660),[254;255];(960,661),[249];(960,663),[247];(960,664),[253];(960,665),[251];(960,666),[258;259];(960,667),[257];(960,668),[261];(960,669),[266;267];(960,670),[265];(960,671),[263];(960,672),[270];(960,674),[272];(960,675),[276];(960,676),[274];(960,679),[291;292];(960,681),[300];(960,682),[306];(960,683),[302];(960,685),[304];(960,686),[308];(960,687),[309;310];(960,688),[312];(960,689),[318];(960,690),[314];(960,691),[316];(960,692),[320];(960,694),[330];(960,696),[334];(960,698),[338];(960,700),[348];(960,704),[365;366];(960,705),[367;368];(960,708),[741];(960,709),[150];(960,713),[215];(960,714),[224];(960,717),[293];(960,718),[322];(960,721),[359];(960,722),[390];(960,732),[682];(960,733),[691];(960,734),[704];(960,735),[742];(960,736),[747];(960,737),[894];(960,738),[151;152];(960,742),[217];(960,743),[226];(960,746),[295];(960,747),[323;324];(960,749),[360];(960,750),[391;392];(960,760),[684];(960,761),[692];(960,763),[705];(960,764),[743];(960,765),[748];(960,766),[895];(960,768),[154];(960,772),[219];(960,773),[228];(960,776),[297];(960,777),[326];(960,780),[394];(960,790),[686];(960,797),[12];(960,802),[528;529];(961,78),[503];(961,118),[972];(961,240),[1165];(961,362),[1357];(961,463),[676];(961,509),[505];(961,710),[157];(961,739),[158];(962,12),[268];(962,24),[703];(962,32),[240];(962,34),[387];(962,43),[514;515];(962,51),[373;374;375;376];(962,61),[347];(962,66),[364];(962,67),[321];(962,73),[869];(962,79),[399];(962,153),[1021];(962,155),[1023];(962,159),[1028];(962,170),[1055];(962,181),[1069];(962,189),[1081];(962,193),[1091];(962,195),[1094];(962,204),[1105];(962,303),[1258];(962,308),[1266];(962,309),[1268];(962,321),[1286];(962,328),[1295];(962,355),[1341];(962,381),[1397];(962,382),[1400];(962,385),[1407];(962,387),[1411];(962,419),[1479];(962,420),[1482];(962,421),[1484];(962,424),[1488];(962,425),[1491];(962,433),[1505];(962,437),[1517];(962,444),[1534];(962,454),[886];(962,455),[405];(962,456),[370];(962,471),[378];(962,476),[389];(962,478),[409];(962,479),[413];(962,480),[417];(962,483),[419];(962,484),[423];(962,486),[425];(962,488),[429];(962,489),[433];(962,491),[435];(962,493),[437];(962,512),[512];(962,517),[532];(962,543),[606];(962,555),[629];(962,564),[660];(962,577),[712];(962,585),[728];(962,586),[726];(962,587),[724];(962,588),[730];(962,594),[757];(962,595),[764];(962,596),[759];(962,600),[769];(962,622),[861];(962,635),[171];(962,636),[173];(962,637),[175];(962,650),[214];(962,652),[212];(962,653),[223];(962,654),[221];(962,655),[229;230];(962,656),[232];(962,657),[239];(962,658),[242];(962,659),[245];(962,660),[254;255];(962,661),[249];(962,663),[247];(962,664),[253];(962,665),[251];(962,666),[258;259];(962,667),[257];(962,668),[261];(962,669),[266;267];(962,670),[265];(962,671),[263];(962,672),[270];(962,674),[272];(962,675),[276];(962,676),[274];(962,679),[291;292];(962,681),[300];(962,682),[306];(962,683),[302];(962,685),[304];(962,686),[308];(962,687),[309;310];(962,688),[312];(962,689),[318];(962,690),[314];(962,691),[316];(962,692),[320];(962,694),[330];(962,696),[334];(962,698),[338];(962,700),[348];(962,704),[366];(962,705),[368];(962,708),[741];(962,709),[150];(962,713),[215];(962,714),[224];(962,717),[293];(962,718),[322];(962,721),[359];(962,722),[390];(962,732),[682];(962,733),[691];(962,734),[704];(962,735),[742];(962,736),[747];(962,737),[894];(962,738),[151;152];(962,742),[217];(962,743),[226];(962,746),[295];(962,747),[323;324];(962,749),[360];(962,750),[391;392];(962,760),[684];(962,761),[692];(962,763),[705];(962,764),[743];(962,765),[748];(962,766),[895];(962,768),[154];(962,772),[219];(962,773),[228];(962,776),[297];(962,777),[326];(962,780),[394];(962,790),[686];(962,797),[12];(962,802),[528;529];(963,7),[196];(963,12),[268];(963,24),[703];(963,32),[240];(963,34),[387];(963,43),[514;515];(963,51),[373;374;375;376];(963,66),[364];(963,67),[321];(963,73),[879];(963,74),[3];(963,79),[399];(963,127),[987];(963,128),[990];(963,129),[992];(963,153),[1021];(963,155),[1023];(963,159),[1028];(963,170),[1055];(963,181),[1069];(963,193),[1091];(963,195),[1094];(963,204),[1105];(963,253),[1186];(963,303),[1258;1259];(963,308),[1266];(963,309),[1268];(963,321),[1286];(963,328),[1295];(963,355),[1341];(963,361),[1355];(963,381),[1397];(963,382),[1400];(963,385),[1407];(963,387),[1411];(963,419),[1479;1480];(963,420),[1482];(963,421),[1484];(963,424),[1488];(963,425),[1491];(963,433),[1505];(963,437),[1517];(963,444),[1534];(963,454),[886];(963,455),[405];(963,456),[370];(963,471),[378];(963,473),[382];(963,476),[389];(963,478),[409];(963,479),[413];(963,480),[417];(963,483),[419];(963,484),[423];(963,486),[425];(963,488),[429];(963,489),[433];(963,491),[435];(963,493),[437];(963,512),[512];(963,517),[532];(963,526),[564];(963,527),[565];(963,543),[606];(963,555),[629];(963,564),[660];(963,577),[712];(963,585),[728];(963,586),[726];(963,587),[724];(963,588),[730];(963,594),[757];(963,595),[764];(963,596),[759];(963,600),[769];(963,612),[820];(963,614),[822];(963,615),[824];(963,616),[826];(963,617),[828];(963,618),[830];(963,622),[861];(963,623),[888];(963,635),[171];(963,636),[173];(963,637),[175];(963,641),[185];(963,642),[190;191];(963,643),[187];(963,644),[189];(963,645),[193];(963,646),[194];(963,647),[197];(963,650),[214];(963,652),[212];(963,653),[223];(963,654),[221];(963,655),[229;230];(963,656),[232];(963,657),[239];(963,658),[242];(963,659),[245];(963,660),[254;255];(963,661),[249];(963,663),[247];(963,664),[253];(963,665),[251];(963,666),[258;259];(963,667),[257];(963,668),[261];(963,669),[266;267];(963,670),[265];(963,671),[263];(963,672),[270];(963,674),[272];(963,675),[276];(963,676),[274];(963,679),[291;292];(963,681),[300];(963,682),[306];(963,683),[302];(963,685),[304];(963,686),[308];(963,687),[309;310];(963,688),[312];(963,689),[318];(963,690),[314];(963,691),[316];(963,692),[320];(963,694),[330];(963,696),[334];(963,698),[338];(963,700),[348];(963,704),[366];(963,705),[368];(963,708),[741];(963,709),[150];(963,713),[215];(963,714),[224];(963,717),[293];(963,718),[322];(963,721),[359];(963,722),[390];(963,732),[682];(963,733),[691];(963,734),[704];(963,735),[742];(963,736),[747];(963,737),[894];(963,738),[151;152];(963,742),[217];(963,743),[226];(963,746),[295];(963,747),[323;324];(963,749),[360];(963,750),[391;392];(963,760),[684];(963,761),[692];(963,763),[705];(963,764),[743];(963,765),[748];(963,766),[895];(963,768),[154];(963,772),[219];(963,773),[228];(963,776),[297];(963,777),[326];(963,780),[394];(963,790),[686];(963,797),[12];(963,802),[528;529];(964,72),[817];(964,355),[1342];(964,433),[1506];(964,513),[88];(964,524),[89];(964,634),[169];(964,737),[894];(964,766),[895];(965,453),[911];(966,72),[801];(966,355),[1342];(966,433),[1506];(966,513),[88];(966,524),[89];(966,634),[169];(966,737),[894];(966,766),[895];(967,95),[943];(967,492),[72];(968,87),[925];(968,414),[1469];(968,415),[1471];(968,416),[1473];(968,417),[1475];(968,418),[1477];(969,12),[268];(969,22),[661];(969,32),[240];(969,34),[387];(969,67),[321];(969,87),[926];(969,153),[1021];(969,155),[1023];(969,159),[1028];(969,170),[1055];(969,250),[1182];(969,264),[1201];(969,275),[1214];(969,289),[1235];(969,414),[1470];(969,415),[1472];(969,416),[1474];(969,417),[1476];(969,418),[1478];(969,454),[886];(969,455),[405];(969,456),[370];(969,460),[601];(969,461),[624];(969,462),[655];(969,471),[378];(969,476),[389];(969,478),[409];(969,479),[413];(969,480),[417];(969,483),[419];(969,484),[423];(969,486),[425];(969,488),[429];(969,489),[433];(969,491),[435];(969,493),[437];(969,512),[512];(969,522),[555];(969,536),[585];(969,537),[587];(969,538),[593];(969,541),[600];(969,549),[615];(969,550),[617];(969,551),[618];(969,553),[623];(969,560),[650];(969,561),[651];(969,562),[654];(969,585),[728];(969,586),[726];(969,587),[724];(969,588),[730];(969,622),[861];(969,650),[214];(969,652),[212];(969,653),[223];(969,654),[221];(969,655),[229;230];(969,656),[232];(969,657),[239];(969,658),[242];(969,659),[245];(969,660),[254;255];(969,661),[249];(969,663),[247];(969,664),[253];(969,665),[251];(969,666),[258;259];(969,667),[257];(969,668),[261];(969,669),[266;267];(969,670),[265];(969,671),[263];(969,672),[270];(969,674),[272];(969,675),[276];(969,676),[274];(969,679),[291;292];(969,681),[300];(969,682),[306];(969,683),[302];(969,685),[304];(969,686),[308];(969,687),[309;310];(969,688),[312];(969,689),[318];(969,690),[314];(969,691),[316];(969,692),[320];(969,694),[330];(969,696),[334];(969,709),[150];(969,713),[215];(969,714),[224];(969,717),[293];(969,718),[322];(969,722),[390];(969,732),[682];(969,738),[152];(969,742),[217];(969,743),[226];(969,746),[295];(969,747),[324];(969,750),[392];(969,760),[684];(969,768),[154];(969,772),[219];(969,773),[228];(969,776),[297];(969,777),[326];(969,780),[394];(969,790),[686];(969,797),[12];(970,12),[268];(970,32),[240];(970,34),[387];(970,43),[514;515];(970,45),[464];(970,64),[440];(970,67),[321];(970,79),[399];(970,153),[1021];(970,155),[1023];(970,159),[1028];(970,170),[1055];(970,204),[1106];(970,295),[1247];(970,390),[1416];(970,454),[886];(970,455),[405];(970,456),[370];(970,471),[378];(970,476),[389];(970,478),[409];(970,479),[413];(970,480),[417];(970,483),[419];(970,484),[423];(970,486),[425];(970,488),[429];(970,489),[433];(970,491),[435];(970,493),[437];(970,512),[512];(970,566),[673];(970,567),[671];(970,569),[675];(970,585),[728];(970,586),[726];(970,587),[724];(970,588),[730];(970,622),[861];(970,650),[214];(970,652),[212];(970,653),[223];(970,654),[221];(970,655),[229;230];(970,656),[232];(970,657),[239];(970,658),[242];(970,659),[245];(970,660),[254;255];(970,661),[249];(970,663),[247];(970,664),[253];(970,665),[251];(970,666),[258;259];(970,667),[257];(970,668),[261];(970,669),[266;267];(970,670),[265];(970,671),[263];(970,672),[270];(970,674),[272];(970,675),[276];(970,676),[274];(970,679),[291;292];(970,681),[300];(970,682),[306];(970,683),[302];(970,685),[304];(970,686),[308];(970,687),[309;310];(970,688),[312];(970,689),[318];(970,690),[314];(970,691),[316];(970,692),[320];(970,694),[330];(970,696),[334];(970,709),[150];(970,713),[215];(970,714),[224];(970,717),[293];(970,718),[322];(970,722),[390];(970,732),[682];(970,738),[152];(970,742),[217];(970,743),[226];(970,746),[295];(970,747),[324];(970,750),[391;392];(970,760),[684];(970,768),[154];(970,772),[219];(970,773),[228];(970,776),[297];(970,777),[326];(970,780),[394];(970,790),[686];(970,797),[12];(971,12),[268];(971,32),[240];(971,34),[387];(971,43),[514;515];(971,46),[463];(971,64),[439];(971,67),[321];(971,79),[399];(971,153),[1021];(971,155),[1023];(971,159),[1028];(971,170),[1055];(971,204),[1106];(971,295),[1247];(971,390),[1416];(971,454),[886];(971,455),[405];(971,456),[370];(971,471),[378];(971,476),[389];(971,478),[409];(971,479),[413];(971,480),[417];(971,483),[419];(971,484),[423];(971,486),[425];(971,488),[429];(971,489),[433];(971,491),[435];(971,493),[437];(971,512),[512];(971,566),[673];(971,567),[671];(971,569),[675];(971,585),[728];(971,586),[726];(971,587),[724];(971,588),[730];(971,622),[861];(971,650),[214];(971,652),[212];(971,653),[223];(971,654),[221];(971,655),[229;230];(971,656),[232];(971,657),[239];(971,658),[242];(971,659),[245];(971,660),[254;255];(971,661),[249];(971,663),[247];(971,664),[253];(971,665),[251];(971,666),[258;259];(971,667),[257];(971,668),[261];(971,669),[266;267];(971,670),[265];(971,671),[263];(971,672),[270];(971,674),[272];(971,675),[276];(971,676),[274];(971,679),[291;292];(971,681),[300];(971,682),[306];(971,683),[302];(971,685),[304];(971,686),[308];(971,687),[309;310];(971,688),[312];(971,689),[318];(971,690),[314];(971,691),[316];(971,692),[320];(971,694),[330];(971,696),[334];(971,709),[150];(971,713),[215];(971,714),[224];(971,717),[293];(971,718),[322];(971,722),[390];(971,732),[682];(971,738),[152];(971,742),[217];(971,743),[226];(971,746),[295];(971,747),[324];(971,750),[391;392];(971,760),[684];(971,768),[154];(971,772),[219];(971,773),[228];(971,776),[297];(971,777),[326];(971,780),[394];(971,790),[686];(971,797),[12];(972,12),[268];(972,32),[240];(972,34),[387];(972,43),[514;515];(972,47),[443];(972,64),[438];(972,67),[321];(972,79),[399];(972,153),[1021];(972,155),[1023];(972,159),[1028];(972,170),[1055];(972,204),[1106];(972,295),[1247];(972,390),[1416];(972,454),[886];(972,455),[405];(972,456),[370];(972,471),[378];(972,476),[389];(972,478),[409];(972,479),[413];(972,480),[417];(972,483),[419];(972,484),[423];(972,486),[425];(972,488),[429];(972,489),[433];(972,491),[435];(972,493),[437];(972,512),[512];(972,566),[673];(972,567),[671];(972,569),[675];(972,585),[728];(972,586),[726];(972,587),[724];(972,588),[730];(972,622),[861];(972,650),[214];(972,652),[212];(972,653),[223];(972,654),[221];(972,655),[229;230];(972,656),[232];(972,657),[239];(972,658),[242];(972,659),[245];(972,660),[254;255];(972,661),[249];(972,663),[247];(972,664),[253];(972,665),[251];(972,666),[258;259];(972,667),[257];(972,668),[261];(972,669),[266;267];(972,670),[265];(972,671),[263];(972,672),[270];(972,674),[272];(972,675),[276];(972,676),[274];(972,679),[291;292];(972,681),[300];(972,682),[306];(972,683),[302];(972,685),[304];(972,686),[308];(972,687),[309;310];(972,688),[312];(972,689),[318];(972,690),[314];(972,691),[316];(972,692),[320];(972,694),[330];(972,696),[334];(972,709),[150];(972,713),[215];(972,714),[224];(972,717),[293];(972,718),[322];(972,722),[390];(972,732),[682];(972,738),[152];(972,742),[217];(972,743),[226];(972,746),[295];(972,747),[324];(972,750),[391;392];(972,760),[684];(972,768),[154];(972,772),[219];(972,773),[228];(972,776),[297];(972,777),[326];(972,780),[394];(972,790),[686];(972,797),[12];(973,94),[940];(973,673),[65];(973,684),[67];(973,695),[68];(974,58),[277];(974,164),[1041];(974,715),[233];(974,744),[234];(975,12),[268];(975,32),[240];(975,34),[387];(975,67),[321];(975,144),[1009];(975,153),[1021];(975,155),[1023];(975,159),[1028];(975,170),[1055];(975,454),[886];(975,455),[405];(975,456),[370];(975,471),[378];(975,476),[389];(975,478),[409];(975,479),[413];(975,480),[417];(975,483),[419];(975,484),[423];(975,486),[425];(975,488),[429];(975,489),[433];(975,491),[435];(975,493),[437];(975,512),[512];(975,585),[728];(975,586),[726];(975,587),[724];(975,588),[730];(975,622),[861];(975,650),[214];(975,652),[212];(975,653),[223];(975,654),[221];(975,655),[229;230];(975,656),[231;232];(975,657),[239];(975,658),[242];(975,659),[245];(975,660),[254;255];(975,661),[249];(975,663),[247];(975,664),[253];(975,665),[251];(975,666),[258;259];(975,667),[257];(975,668),[261];(975,669),[266;267];(975,670),[265];(975,671),[263];(975,672),[270];(975,674),[272];(975,675),[276];(975,676),[274];(975,679),[291;292];(975,681),[300];(975,682),[306];(975,683),[302];(975,685),[304];(975,686),[308];(975,687),[309;310];(975,688),[312];(975,689),[318];(975,690),[314];(975,691),[316];(975,692),[320];(975,694),[330];(975,696),[334];(975,709),[150];(975,713),[215];(975,714),[224];(975,717),[293];(975,718),[322];(975,722),[390];(975,732),[682];(975,738),[152];(975,742),[217];(975,743),[226];(975,746),[295];(975,747),[324];(975,750),[392];(975,760),[684];(975,768),[154];(975,772),[219];(975,773),[228];(975,776),[297];(975,777),[326];(975,780),[394];(975,790),[686];(975,797),[12];(976,12),[268];(976,32),[240];(976,34),[387];(976,67),[321];(976,153),[1021];(976,155),[1023];(976,159),[1028];(976,170),[1055];(976,196),[1095];(976,354),[1339];(976,386),[1408];(976,396),[1429];(976,441),[1526];(976,454),[882;886];(976,455),[405];(976,456),[369;370];(976,471),[378];(976,476),[389];(976,478),[409];(976,479),[413];(976,480),[417];(976,483),[419];(976,484),[423];(976,486),[425];(976,488),[429];(976,489),[433];(976,491),[435];(976,493),[437];(976,512),[512];(976,585),[728];(976,586),[726];(976,587),[724];(976,588),[730];(976,622),[861];(976,650),[214];(976,652),[212];(976,653),[223];(976,654),[221];(976,655),[229;230];(976,656),[232];(976,657),[239];(976,658),[242];(976,659),[245];(976,660),[254;255];(976,661),[249];(976,663),[247];(976,664),[253];(976,665),[251];(976,666),[258;259];(976,667),[257];(976,668),[261];(976,669),[266;267];(976,670),[265];(976,671),[263];(976,672),[270];(976,674),[272];(976,675),[276];(976,676),[274];(976,679),[291;292];(976,681),[300];(976,682),[306];(976,683),[302];(976,685),[304];(976,686),[308];(976,687),[309;310];(976,688),[312];(976,689),[318];(976,690),[314];(976,691),[316];(976,692),[320];(976,694),[330];(976,696),[334];(976,709),[150];(976,713),[215];(976,714),[224];(976,717),[293];(976,718),[322];(976,722),[390];(976,732),[682];(976,738),[152];(976,742),[217];(976,743),[226];(976,746),[295];(976,747),[324];(976,750),[392];(976,760),[684];(976,768),[154];(976,772),[219];(976,773),[228];(976,776),[297];(976,777),[326];(976,780),[394];(976,790),[686];(976,797),[12];(977,12),[268];(977,32),[240];(977,34),[387];(977,48),[354];(977,58),[277];(977,67),[321];(977,137),[1002];(977,153),[1021];(977,155),[1023];(977,159),[1028];(977,164),[1047];(977,170),[1055];(977,192),[1090];(977,373),[1380];(977,454),[886];(977,455),[405];(977,456),[370];(977,468),[351];(977,471),[378];(977,476),[389];(977,478),[409];(977,479),[413];(977,480),[417];(977,483),[419];(977,484),[423];(977,486),[425];(977,488),[429];(977,489),[433];(977,491),[435];(977,493),[437];(977,512),[512];(977,585),[728];(977,586),[726];(977,587),[724];(977,588),[730];(977,622),[861];(977,650),[213;214];(977,652),[212];(977,653),[223];(977,654),[221];(977,655),[229;230];(977,656),[232];(977,657),[239];(977,658),[242];(977,659),[245];(977,660),[254;255];(977,661),[249];(977,663),[247];(977,664),[253];(977,665),[251];(977,666),[258;259];(977,667),[257];(977,668),[261];(977,669),[266;267];(977,670),[265];(977,671),[263];(977,672),[270];(977,674),[272];(977,675),[276];(977,676),[274];(977,679),[291;292];(977,681),[300];(977,682),[306];(977,683),[302];(977,685),[304];(977,686),[308];(977,687),[309;310];(977,688),[312];(977,689),[318];(977,690),[314];(977,691),[316];(977,692),[320];(977,694),[330];(977,696),[334];(977,699),[346];(977,701),[352;353];(977,702),[357];(977,709),[150];(977,713),[215];(977,714),[224];(977,715),[233];(977,717),[293];(977,718),[322];(977,720),[340];(977,722),[390];(977,732),[682];(977,738),[152];(977,742),[217];(977,743),[226];(977,744),[234];(977,746),[295];(977,747),[324];(977,748),[341;342];(977,750),[392];(977,760),[684];(977,768),[154];(977,772),[219];(977,773),[228];(977,776),[297];(977,777),[326];(977,778),[344];(977,780),[394];(977,790),[686];(977,797),[12];(978,104),[954];(978,327),[1293];(978,513),[88];(978,524),[90];(978,535),[92];(978,546),[94];(978,557),[95];(978,597),[760];(978,612),[820];(978,614),[822];(978,615),[824];(978,616),[826];(978,617),[828];(978,618),[830];(979,515),[517];(980,12),[268];(980,32),[240];(980,34),[387];(980,49),[698];(980,67),[321];(980,104),[953];(980,153),[1021];(980,155),[1023];(980,159),[1028];(980,170),[1055];(980,296),[1249];(980,307),[1265];(980,327),[1294];(980,454),[886];(980,455),[405];(980,456),[370];(980,471),[378];(980,476),[389];(980,478),[409];(980,479),[413];(980,480),[417];(980,483),[419];(980,484),[423];(980,486),[425];(980,488),[429];(980,489),[433];(980,491),[435];(980,493),[437];(980,512),[512];(980,513),[88];(980,524),[90];(980,535),[92];(980,546),[94];(980,557),[95];(980,570),[679];(980,571),[681];(980,572),[687];(980,575),[700];(980,576),[701];(980,585),[728];(980,586),[726];(980,587),[724];(980,588),[730];(980,597),[760];(980,608),[787];(980,612),[820];(980,614),[822];(980,615),[824];(980,616),[826];(980,617),[828];(980,618),[830];(980,622),[861];(980,650),[214];(980,652),[212];(980,653),[223];(980,654),[221];(980,655),[229;230];(980,656),[232];(980,657),[239];(980,658),[242];(980,659),[245];(980,660),[254;255];(980,661),[249];(980,663),[247];(980,664),[253];(980,665),[251];(980,666),[258;259];(980,667),[257];(980,668),[261];(980,669),[266;267];(980,670),[265];(980,671),[263];(980,672),[270];(980,674),[272];(980,675),[276];(980,676),[274];(980,679),[291;292];(980,681),[300];(980,682),[306];(980,683),[302];(980,685),[304];(980,686),[308];(980,687),[309;310];(980,688),[312];(980,689),[318];(980,690),[314];(980,691),[316];(980,692),[320];(980,694),[330];(980,696),[334];(980,709),[150];(980,713),[215];(980,714),[224];(980,717),[293];(980,718),[322];(980,722),[390];(980,732),[682];(980,738),[152];(980,742),[217];(980,743),[226];(980,746),[295];(980,747),[324];(980,750),[392];(980,760),[684];(980,768),[154];(980,772),[219];(980,773),[228];(980,776),[297];(980,777),[326];(980,780),[394];(980,790),[686];(980,797),[12];(981,0),[339];(981,12),[268];(981,24),[703];(981,32),[240];(981,34),[387];(981,43),[514;515];(981,51),[373;374;375;376];(981,66),[364];(981,67),[321];(981,73),[868];(981,79),[399];(981,153),[1021];(981,155),[1023];(981,159),[1028];(981,170),[1055];(981,181),[1069];(981,193),[1091];(981,195),[1094];(981,204),[1105];(981,303),[1258];(981,308),[1266];(981,309),[1268];(981,321),[1286];(981,328),[1295];(981,355),[1341];(981,381),[1397];(981,382),[1400];(981,385),[1407];(981,387),[1411];(981,419),[1479];(981,420),[1482];(981,421),[1484];(981,424),[1488];(981,425),[1491];(981,433),[1505];(981,437),[1517];(981,444),[1534];(981,454),[886];(981,455),[405];(981,456),[370];(981,471),[378];(981,476),[389];(981,478),[409];(981,479),[413];(981,480),[417];(981,483),[419];(981,484),[423];(981,486),[425];(981,488),[429];(981,489),[433];(981,491),[435];(981,493),[437];(981,512),[512];(981,517),[532];(981,543),[606];(981,555),[629];(981,564),[660];(981,577),[712];(981,585),[728];(981,586),[726];(981,587),[724];(981,588),[730];(981,594),[757];(981,595),[764];(981,596),[759];(981,600),[769];(981,622),[861];(981,635),[171];(981,636),[173];(981,637),[175];(981,650),[214];(981,652),[212];(981,653),[223];(981,654),[221];(981,655),[229;230];(981,656),[232];(981,657),[239];(981,658),[242];(981,659),[245];(981,660),[254;255];(981,661),[249];(981,663),[247];(981,664),[253];(981,665),[251];(981,666),[258;259];(981,667),[257];(981,668),[261];(981,669),[266;267];(981,670),[265];(981,671),[263];(981,672),[270];(981,674),[272];(981,675),[276];(981,676),[274];(981,679),[291;292];(981,681),[300];(981,682),[306];(981,683),[302];(981,685),[304];(981,686),[308];(981,687),[309;310];(981,688),[312];(981,689),[318];(981,690),[314];(981,691),[316];(981,692),[320];(981,694),[330];(981,696),[334];(981,698),[338];(981,700),[348];(981,704),[366];(981,705),[368];(981,708),[741];(981,709),[150];(981,713),[215];(981,714),[224];(981,717),[293];(981,718),[322];(981,721),[359];(981,722),[390];(981,732),[682];(981,733),[691];(981,734),[704];(981,735),[742];(981,736),[747];(981,737),[894];(981,738),[151;152];(981,742),[217];(981,743),[226];(981,746),[295];(981,747),[323;324];(981,749),[360];(981,750),[391;392];(981,760),[684];(981,761),[692];(981,763),[705];(981,764),[743];(981,765),[748];(981,766),[895];(981,768),[154];(981,772),[219];(981,773),[228];(981,776),[297];(981,777),[326];(981,780),[394];(981,790),[686];(981,797),[12];(981,802),[528;529];(982,1),[44];(982,5),[893];(982,7),[196];(982,9),[183];(982,10),[562];(982,12),[268];(982,13),[283];(982,24),[703];(982,29),[739];(982,30),[738];(982,32),[240];(982,34),[387];(982,35),[9];(982,37),[6];(982,43),[514;515];(982,44),[717];(982,51),[373;374;375;376];(982,54),[132;133];(982,63),[28];(982,66),[364];(982,67),[321];(982,69),[55];(982,70),[53];(982,73),[875;876];(982,79),[398;399;400];(982,89),[930];(982,99),[948];(982,100),[949];(982,101),[950];(982,102),[951];(982,106),[957];(982,110),[961];(982,111),[962];(982,120),[975;976];(982,122),[980];(982,127),[987];(982,128),[989;990];(982,138),[1003];(982,153),[1021];(982,155),[1023];(982,159),[1028];(982,167),[1050;1051];(982,170),[1055];(982,181),[1068;1069;1070];(982,183),[1073];(982,185),[1075];(982,190),[1085];(982,193),[1091];(982,195),[1094];(982,204),[1105];(982,206),[1108];(982,208),[1110];(982,211),[1113];(982,215),[1118];(982,216),[1119];(982,223),[1128];(982,227),[1134];(982,247),[1179];(982,295),[1248];(982,296),[1249];(982,302),[1256];(982,303),[1258];(982,308),[1266;1267];(982,309),[1268];(982,316),[1278];(982,319),[1284];(982,321),[1286];(982,328),[1295];(982,344),[1317;1318];(982,355),[1341];(982,363),[1359;1360];(982,366),[1366];(982,367),[1367];(982,381),[1397;1398];(982,382),[1399;1400;1401];(982,384),[1405;1406];(982,385),[1407];(982,387),[1411];(982,392),[1420];(982,404),[1447];(982,419),[1479];(982,420),[1482;1483];(982,421),[1484];(982,422),[1486];(982,423),[1487];(982,424),[1488];(982,425),[1489;1491];(982,433),[1505];(982,434),[1508];(982,436),[1516];(982,437),[1517;1518];(982,438),[1519];(982,444),[1534;1535];(982,445),[1537];(982,446),[1540];(982,447),[15];(982,448),[22];(982,450),[899];(982,454),[886];(982,455),[405];(982,456),[370];(982,464),[32];(982,465),[114;115];(982,466),[142];(982,467),[181];(982,468),[350];(982,471),[378];(982,476),[389];(982,477),[407];(982,478),[409];(982,479),[413];(982,480),[416;417];(982,481),[80];(982,482),[415];(982,483),[419];(982,484),[422;423];(982,485),[421];(982,486),[425];(982,487),[427];(982,488),[429];(982,489),[432;433];(982,490),[431];(982,491),[435];(982,492),[73];(982,493),[437];(982,494),[444;445];(982,495),[446;447];(982,503),[82;83];(982,512),[512];(982,513),[87];(982,517),[532];(982,522),[556];(982,523),[558];(982,525),[560];(982,543),[606];(982,555),[629];(982,564),[660];(982,566),[673];(982,567),[671];(982,569),[675];(982,570),[679];(982,571),[681];(982,572),[688];(982,573),[689;690];(982,574),[697];(982,575),[700];(982,576),[702];(982,577),[712];(982,579),[31];(982,580),[102;103];(982,581),[716];(982,583),[721];(982,585),[727;728];(982,586),[726];(982,587),[724];(982,588),[730];(982,589),[733];(982,590),[735];(982,592),[753];(982,593),[755];(982,594),[757];(982,595),[764;765];(982,596),[759];(982,597),[761];(982,598),[763];(982,599),[767];(982,600),[769];(982,603),[777];(982,604),[779];(982,609),[794];(982,610),[796];(982,611),[798];(982,612),[820];(982,614),[822];(982,615),[824];(982,616),[826];(982,617),[828];(982,618),[830];(982,622),[861];(982,624),[113];(982,627),[117];(982,628),[137];(982,629),[48];(982,630),[141];(982,631),[146];(982,633),[156];(982,635),[171];(982,636),[173];(982,637),[175];(982,639),[179];(982,640),[50];(982,641),[185];(982,642),[190;191];(982,643),[187];(982,644),[189];(982,645),[193];(982,646),[194;195];(982,647),[198];(982,648),[200];(982,650),[214];(982,651),[52];(982,652),[212];(982,653),[223];(982,654),[221];(982,655),[229;230];(982,656),[232];(982,657),[239];(982,658),[242];(982,659),[245];(982,660),[254;255];(982,661),[249];(982,663),[247];(982,664),[253];(982,665),[251];(982,666),[258;259];(982,667),[257];(982,668),[261];(982,669),[266;267];(982,670),[265];(982,671),[263];(982,672),[270];(982,674),[272];(982,675),[276];(982,676),[274];(982,679),[291;292];(982,681),[300];(982,682),[306];(982,683),[302];(982,685),[304];(982,686),[308];(982,687),[309;310];(982,688),[312];(982,689),[318];(982,690),[314];(982,691),[316];(982,692),[320];(982,693),[327;328];(982,694),[330];(982,696),[333;334];(982,697),[332];(982,698),[338];(982,700),[348];(982,701),[352];(982,704),[366];(982,705),[368];(982,706),[8];(982,708),[741];(982,709),[150];(982,710),[157];(982,711),[163];(982,712),[206];(982,713),[215];(982,714),[224];(982,717),[293];(982,718),[322];(982,721),[359];(982,722),[390];(982,723),[466];(982,726),[544];(982,730),[74];(982,732),[682];(982,733),[691];(982,734),[704];(982,735),[742];(982,736),[747];(982,737),[894];(982,738),[151;152];(982,739),[159];(982,740),[164;165];(982,741),[207];(982,742),[216;217];(982,743),[226];(982,746),[295];(982,747),[323;324];(982,749),[360];(982,750),[391;392];(982,752),[467];(982,755),[545];(982,760),[684];(982,761),[692];(982,762),[75;76];(982,763),[705;706];(982,764),[743;744];(982,765),[748];(982,766),[895];(982,767),[10];(982,768),[154];(982,769),[161];(982,770),[167];(982,772),[219];(982,773),[228];(982,776),[297];(982,777),[326];(982,780),[394];(982,790),[686];(982,792),[708];(982,793),[746];(982,795),[78];(982,797),[12];(982,798),[84;85];(982,802),[528;529];(982,806),[29];(983,39),[666];(983,294),[1246];(983,728),[588];(983,757),[589];(984,78),[503];(984,240),[1164];(984,463),[676];(984,509),[505];(985,58),[277];(985,164),[1040];(985,715),[233];(985,744),[234];(986,453),[910];(987,12),[268];(987,32),[240];(987,34),[387];(987,67),[321];(987,153),[1021];(987,155),[1023];(987,159),[1028];(987,170),[1055];(987,188),[1080];(987,190),[1087];(987,454),[886];(987,455),[405];(987,456),[370];(987,468),[350];(987,471),[378];(987,476),[389];(987,478),[409];(987,479),[413];(987,480),[417];(987,483),[419];(987,484),[423];(987,486),[425];(987,488),[429];(987,489),[433];(987,491),[435];(987,493),[437];(987,512),[512];(987,585),[728];(987,586),[726];(987,587),[724];(987,588),[730];(987,622),[861];(987,650),[214];(987,652),[212];(987,653),[223];(987,654),[221];(987,655),[229;230];(987,656),[232];(987,657),[239];(987,658),[242];(987,659),[245];(987,660),[254;255];(987,661),[249];(987,663),[247];(987,664),[253];(987,665),[251];(987,666),[258;259];(987,667),[257];(987,668),[261];(987,669),[266;267];(987,670),[265];(987,671),[263];(987,672),[270];(987,674),[272];(987,675),[276];(987,676),[274];(987,679),[291;292];(987,681),[300];(987,682),[306];(987,683),[302];(987,685),[304];(987,686),[308];(987,687),[309;310];(987,688),[312];(987,689),[318];(987,690),[314];(987,691),[316];(987,692),[320];(987,694),[330];(987,696),[334];(987,699),[345];(987,701),[352];(987,709),[150];(987,713),[215];(987,714),[224];(987,717),[293];(987,718),[322];(987,720),[340];(987,722),[390];(987,732),[682];(987,738),[152];(987,742),[217];(987,743),[226];(987,746),[295];(987,747),[324];(987,748),[341];(987,750),[392];(987,760),[684];(987,768),[154];(987,772),[219];(987,773),[228];(987,776),[297];(987,777),[326];(987,780),[394];(987,790),[686];(987,797),[12];(988,147),[1012];(989,12),[268];(989,32),[240];(989,34),[387];(989,67),[321];(989,153),[1021];(989,155),[1023];(989,159),[1028];(989,170),[1055];(989,241),[1171];(989,454),[886];(989,455),[405];(989,456),[370];(989,471),[378];(989,476),[389];(989,478),[409];(989,479),[413];(989,480),[417];(989,483),[419];(989,484),[423];(989,486),[425];(989,488),[429];(989,489),[433];(989,491),[435];(989,493),[437];(989,511),[509];(989,512),[512];(989,567),[670];(989,585),[728];(989,586),[726];(989,587),[724];(989,588),[730];(989,622),[861];(989,650),[214];(989,652),[212];(989,653),[223];(989,654),[221];(989,655),[229;230];(989,656),[232];(989,657),[239];(989,658),[242];(989,659),[245];(989,660),[254;255];(989,661),[249];(989,663),[247];(989,664),[253];(989,665),[251];(989,666),[258;259];(989,667),[257];(989,668),[261];(989,669),[266;267];(989,670),[265];(989,671),[263];(989,672),[270];(989,674),[272];(989,675),[276];(989,676),[274];(989,679),[291;292];(989,681),[300];(989,682),[306];(989,683),[301;302];(989,685),[304];(989,686),[308];(989,687),[309;310];(989,688),[312];(989,689),[318];(989,690),[314];(989,691),[316];(989,692),[320];(989,694),[330];(989,696),[334];(989,709),[150];(989,713),[215];(989,714),[224];(989,717),[293];(989,718),[322];(989,722),[390];(989,732),[682];(989,738),[152];(989,742),[217];(989,743),[226];(989,746),[295];(989,747),[324];(989,750),[392];(989,760),[684];(989,768),[154];(989,772),[219];(989,773),[228];(989,776),[297];(989,777),[326];(989,780),[394];(989,790),[686];(989,797),[12];(990,12),[268];(990,32),[240];(990,34),[387];(990,67),[321];(990,153),[1021];(990,155),[1023];(990,159),[1028];(990,170),[1055];(990,271),[1208];(990,454),[886];(990,455),[405];(990,456),[370];(990,471),[378];(990,476),[389];(990,478),[409];(990,479),[413];(990,480),[417];(990,483),[419];(990,484),[423];(990,486),[425];(990,488),[429];(990,489),[433];(990,491),[435];(990,493),[437];(990,512),[512];(990,544),[610];(990,585),[728];(990,586),[726];(990,587),[724];(990,588),[730];(990,622),[861];(990,650),[214];(990,652),[212];(990,653),[223];(990,654),[221];(990,655),[229;230];(990,656),[232];(990,657),[239];(990,658),[242];(990,659),[245];(990,660),[254;255];(990,661),[249];(990,663),[247];(990,664),[253];(990,665),[251];(990,666),[258;259];(990,667),[257];(990,668),[261];(990,669),[266;267];(990,670),[265];(990,671),[263];(990,672),[270];(990,674),[272];(990,675),[276];(990,676),[274];(990,679),[291;292];(990,681),[300];(990,682),[306];(990,683),[302];(990,685),[304];(990,686),[308];(990,687),[309;310];(990,688),[312];(990,689),[318];(990,690),[314];(990,691),[316];(990,692),[320];(990,694),[330];(990,696),[334];(990,709),[150];(990,713),[215];(990,714),[224];(990,717),[293];(990,718),[322];(990,722),[390];(990,732),[682];(990,738),[152];(990,742),[217];(990,743),[226];(990,746),[295];(990,747),[324];(990,750),[392];(990,760),[684];(990,768),[154];(990,772),[219];(990,773),[228];(990,776),[297];(990,777),[326];(990,780),[394];(990,790),[686];(990,797),[12];(991,12),[268];(991,32),[240];(991,34),[387];(991,67),[321];(991,153),[1021];(991,155),[1023];(991,159),[1028];(991,170),[1055];(991,454),[886];(991,455),[402;405];(991,456),[370];(991,471),[378];(991,476),[389];(991,478),[409];(991,479),[413];(991,480),[417];(991,483),[419];(991,484),[423];(991,486),[425];(991,488),[429];(991,489),[433];(991,491),[435];(991,493),[437];(991,512),[512];(991,585),[728];(991,586),[726];(991,587),[724];(991,588),[730];(991,622),[861];(991,650),[214];(991,652),[212];(991,653),[223];(991,654),[221];(991,655),[229;230];(991,656),[232];(991,657),[239];(991,658),[242];(991,659),[245];(991,660),[254;255];(991,661),[249];(991,663),[247];(991,664),[253];(991,665),[251];(991,666),[258;259];(991,667),[257];(991,668),[261];(991,669),[266;267];(991,670),[265];(991,671),[263];(991,672),[270];(991,674),[272];(991,675),[276];(991,676),[274];(991,679),[291;292];(991,681),[300];(991,682),[306];(991,683),[302];(991,685),[304];(991,686),[308];(991,687),[309;310];(991,688),[312];(991,689),[318];(991,690),[314];(991,691),[316];(991,692),[320];(991,694),[330];(991,696),[334];(991,709),[150];(991,713),[215];(991,714),[224];(991,717),[293];(991,718),[322];(991,722),[390];(991,732),[682];(991,738),[152];(991,742),[217];(991,743),[226];(991,746),[295];(991,747),[324];(991,750),[392];(991,760),[684];(991,768),[154];(991,772),[219];(991,773),[228];(991,776),[297];(991,777),[326];(991,780),[394];(991,790),[686];(991,797),[12];(993,7),[196];(993,8),[581];(993,10),[561];(993,12),[268];(993,32),[240];(993,34),[387];(993,67),[321];(993,76),[202];(993,77),[632];(993,122),[979];(993,127),[987];(993,128),[989;990];(993,130),[994];(993,131),[995];(993,153),[1021];(993,155),[1023];(993,159),[1028];(993,170),[1055];(993,247),[1179];(993,259),[1194];(993,280),[1221];(993,281),[1224];(993,365),[1364];(993,404),[1447];(993,407),[1454];(993,410),[1462];(993,411),[1464];(993,454),[886];(993,455),[405];(993,456),[370];(993,459),[579;580];(993,460),[602];(993,461),[625];(993,467),[181];(993,471),[378];(993,476),[389];(993,478),[409];(993,479),[413];(993,480),[417];(993,483),[419];(993,484),[423];(993,486),[425];(993,488),[429];(993,489),[433];(993,491),[435];(993,493),[437];(993,512),[512];(993,526),[564];(993,527),[566];(993,528),[570];(993,530),[576];(993,531),[572];(993,532),[574];(993,533),[578];(993,534),[583];(993,536),[585];(993,537),[587];(993,538),[594];(993,541),[600];(993,542),[604];(993,544),[611];(993,545),[609];(993,547),[630;631];(993,548),[613];(993,549),[615];(993,550),[617];(993,551),[619];(993,553),[623];(993,554),[627];(993,556),[644];(993,585),[728];(993,586),[726];(993,587),[724];(993,588),[730];(993,612),[820];(993,614),[822];(993,615),[824];(993,616),[826];(993,617),[828];(993,618),[830];(993,622),[861];(993,641),[185];(993,642),[190;191];(993,643),[187];(993,644),[189];(993,645),[193];(993,646),[194;195];(993,647),[198];(993,648),[199;200];(993,650),[214];(993,652),[212];(993,653),[223];(993,654),[221];(993,655),[229;230];(993,656),[232];(993,657),[239];(993,658),[242];(993,659),[245];(993,660),[254;255];(993,661),[249];(993,663),[247];(993,664),[253];(993,665),[251];(993,666),[258;259];(993,667),[257];(993,668),[261];(993,669),[266;267];(993,670),[265];(993,671),[263];(993,672),[270];(993,674),[272];(993,675),[276];(993,676),[274];(993,679),[291;292];(993,681),[300];(993,682),[306];(993,683),[302];(993,685),[304];(993,686),[308];(993,687),[309;310];(993,688),[312];(993,689),[318];(993,690),[314];(993,691),[316];(993,692),[320];(993,694),[330];(993,696),[334];(993,709),[150];(993,713),[215];(993,714),[224];(993,717),[293];(993,718),[322];(993,722),[390];(993,726),[544];(993,727),[539];(993,732),[682];(993,738),[152];(993,742),[217];(993,743),[226];(993,746),[295];(993,747),[324];(993,750),[392];(993,755),[545;546];(993,756),[540];(993,760),[684];(993,768),[154];(993,772),[219];(993,773),[228];(993,776),[297];(993,777),[326];(993,780),[394];(993,785),[548];(993,790),[686];(993,797),[12];(994,12),[268];(994,32),[240];(994,34),[387];(994,53),[135];(994,54),[118];(994,67),[321];(994,110),[961];(994,111),[962];(994,153),[1021];(994,155),[1023];(994,159),[1028];(994,170),[1055];(994,296),[1249];(994,328),[1295];(994,425),[1489];(994,454),[886];(994,455),[405];(994,456),[370];(994,465),[114;115];(994,466),[142];(994,471),[378];(994,476),[389];(994,478),[409];(994,479),[413];(994,480),[417];(994,481),[80];(994,483),[419];(994,484),[423];(994,486),[425];(994,488),[429];(994,489),[433];(994,491),[435];(994,492),[73];(994,493),[437];(994,512),[512];(994,522),[556];(994,523),[558];(994,525),[560];(994,570),[679];(994,571),[681];(994,572),[688];(994,573),[690];(994,574),[697];(994,575),[700];(994,576),[702];(994,577),[712];(994,581),[716];(994,585),[728];(994,586),[726];(994,587),[724];(994,588),[730];(994,589),[733];(994,590),[735];(994,594),[757];(994,595),[764;765];(994,596),[759];(994,597),[761];(994,598),[763];(994,599),[767];(994,603),[777];(994,604),[779];(994,609),[794];(994,610),[796];(994,611),[798];(994,612),[820];(994,614),[822];(994,615),[824];(994,616),[826];(994,617),[828];(994,618),[830];(994,622),[861];(994,624),[113];(994,627),[117];(994,628),[137];(994,629),[48];(994,630),[141];(994,631),[146];(994,633),[156];(994,639),[179];(994,640),[50];(994,650),[214];(994,651),[52];(994,652),[212];(994,653),[223];(994,654),[221];(994,655),[229;230];(994,656),[232];(994,657),[239];(994,658),[242];(994,659),[245];(994,660),[254;255];(994,661),[249];(994,663),[247];(994,664),[253];(994,665),[251];(994,666),[258;259];(994,667),[257];(994,668),[261];(994,669),[266;267];(994,670),[265];(994,671),[263];(994,672),[270];(994,674),[272];(994,675),[276];(994,676),[274];(994,679),[291;292];(994,681),[300];(994,682),[306];(994,683),[302];(994,685),[304];(994,686),[308];(994,687),[309;310];(994,688),[312];(994,689),[318];(994,690),[314];(994,691),[316];(994,692),[320];(994,694),[330];(994,696),[334];(994,708),[741];(994,709),[150];(994,710),[157];(994,711),[163];(994,713),[215];(994,714),[224];(994,717),[293];(994,718),[322];(994,722),[390];(994,730),[74];(994,732),[682];(994,734),[704];(994,735),[742];(994,738),[152];(994,739),[159];(994,740),[165];(994,742),[217];(994,743),[226];(994,746),[295];(994,747),[324];(994,750),[392];(994,760),[684];(994,762),[76];(994,763),[706];(994,764),[743;744];(994,768),[154];(994,769),[161];(994,770),[167];(994,772),[219];(994,773),[228];(994,776),[297];(994,777),[326];(994,780),[394];(994,790),[686];(994,792),[708];(994,793),[746];(994,795),[78];(994,797),[12];(995,350),[1331];(995,351),[1333];(995,619),[856];(995,620),[854];(996,98),[947];(996,105),[955];(997,98),[946];(997,105),[956];(998,357),[1346];(999,453),[917];(1000,12),[268];(1000,32),[240];(1000,34),[387];(1000,54),[128];(1000,60),[147];(1000,67),[321];(1000,110),[961];(1000,111),[962];(1000,153),[1021];(1000,155),[1023];(1000,159),[1028];(1000,170),[1055];(1000,296),[1249];(1000,328),[1295];(1000,425),[1489];(1000,454),[886];(1000,455),[405];(1000,456),[370];(1000,465),[114;115];(1000,466),[142];(1000,471),[378];(1000,476),[389];(1000,478),[409];(1000,479),[413];(1000,480),[417];(1000,481),[80];(1000,483),[419];(1000,484),[423];(1000,486),[425];(1000,488),[429];(1000,489),[433];(1000,491),[435];(1000,492),[73];(1000,493),[437];(1000,512),[512];(1000,522),[556];(1000,523),[558];(1000,525),[560];(1000,570),[679];(1000,571),[681];(1000,572),[688];(1000,573),[690];(1000,574),[697];(1000,575),[700];(1000,576),[702];(1000,577),[712];(1000,581),[716];(1000,585),[728];(1000,586),[726];(1000,587),[724];(1000,588),[730];(1000,589),[733];(1000,590),[735];(1000,594),[757];(1000,595),[764;765];(1000,596),[759];(1000,597),[761];(1000,598),[763];(1000,599),[767];(1000,603),[777];(1000,604),[779];(1000,609),[794];(1000,610),[796];(1000,611),[798];(1000,612),[820];(1000,614),[822];(1000,615),[824];(1000,616),[826];(1000,617),[828];(1000,618),[830];(1000,622),[861];(1000,624),[113];(1000,627),[117];(1000,628),[137];(1000,629),[48];(1000,630),[141];(1000,631),[146];(1000,633),[156];(1000,639),[179];(1000,640),[50];(1000,650),[214];(1000,651),[52];(1000,652),[212];(1000,653),[223];(1000,654),[221];(1000,655),[229;230];(1000,656),[232];(1000,657),[239];(1000,658),[242];(1000,659),[245];(1000,660),[254;255];(1000,661),[249];(1000,663),[247];(1000,664),[253];(1000,665),[251];(1000,666),[258;259];(1000,667),[257];(1000,668),[261];(1000,669),[266;267];(1000,670),[265];(1000,671),[263];(1000,672),[270];(1000,674),[272];(1000,675),[276];(1000,676),[274];(1000,679),[291;292];(1000,681),[300];(1000,682),[306];(1000,683),[302];(1000,685),[304];(1000,686),[308];(1000,687),[309;310];(1000,688),[312];(1000,689),[318];(1000,690),[314];(1000,691),[316];(1000,692),[320];(1000,694),[330];(1000,696),[334];(1000,708),[741];(1000,709),[150];(1000,710),[157];(1000,711),[163];(1000,713),[215];(1000,714),[224];(1000,717),[293];(1000,718),[322];(1000,722),[390];(1000,730),[74];(1000,732),[682];(1000,734),[704];(1000,735),[742];(1000,738),[152];(1000,739),[159];(1000,740),[165];(1000,742),[217];(1000,743),[226];(1000,746),[295];(1000,747),[324];(1000,750),[392];(1000,760),[684];(1000,762),[76];(1000,763),[706];(1000,764),[743;744];(1000,768),[154];(1000,769),[161];(1000,770),[167];(1000,772),[219];(1000,773),[228];(1000,776),[297];(1000,777),[326];(1000,780),[394];(1000,790),[686];(1000,792),[708];(1000,793),[746];(1000,795),[78];(1000,797),[12];(1001,12),[268];(1001,24),[703];(1001,32),[240];(1001,34),[387];(1001,43),[514;515];(1001,51),[373;374;375;376];(1001,61),[347];(1001,66),[364];(1001,67),[321];(1001,73),[869];(1001,79),[399];(1001,153),[1021];(1001,155),[1023];(1001,159),[1028];(1001,170),[1055];(1001,181),[1069];(1001,189),[1084];(1001,193),[1091];(1001,195),[1094];(1001,204),[1105];(1001,303),[1258];(1001,308),[1266];(1001,309),[1268];(1001,321),[1286];(1001,328),[1295];(1001,355),[1341];(1001,381),[1397];(1001,382),[1400];(1001,385),[1407];(1001,387),[1411];(1001,419),[1479];(1001,420),[1482];(1001,421),[1484];(1001,424),[1488];(1001,425),[1491];(1001,433),[1505];(1001,437),[1517];(1001,444),[1534];(1001,454),[886];(1001,455),[405];(1001,456),[370];(1001,471),[378];(1001,476),[389];(1001,478),[409];(1001,479),[413];(1001,480),[417];(1001,483),[419];(1001,484),[423];(1001,486),[425];(1001,488),[429];(1001,489),[433];(1001,491),[435];(1001,493),[437];(1001,512),[512];(1001,517),[532];(1001,543),[606];(1001,555),[629];(1001,564),[660];(1001,577),[712];(1001,585),[728];(1001,586),[726];(1001,587),[724];(1001,588),[730];(1001,594),[757];(1001,595),[764];(1001,596),[759];(1001,600),[769];(1001,622),[861];(1001,635),[171];(1001,636),[173];(1001,637),[175];(1001,650),[214];(1001,652),[212];(1001,653),[223];(1001,654),[221];(1001,655),[229;230];(1001,656),[232];(1001,657),[239];(1001,658),[242];(1001,659),[245];(1001,660),[254;255];(1001,661),[249];(1001,663),[247];(1001,664),[253];(1001,665),[251];(1001,666),[258;259];(1001,667),[257];(1001,668),[261];(1001,669),[266;267];(1001,670),[265];(1001,671),[263];(1001,672),[270];(1001,674),[272];(1001,675),[276];(1001,676),[274];(1001,679),[291;292];(1001,681),[300];(1001,682),[306];(1001,683),[302];(1001,685),[304];(1001,686),[308];(1001,687),[309;310];(1001,688),[312];(1001,689),[318];(1001,690),[314];(1001,691),[316];(1001,692),[320];(1001,694),[330];(1001,696),[334];(1001,698),[338];(1001,700),[348];(1001,704),[366];(1001,705),[368];(1001,708),[741];(1001,709),[150];(1001,713),[215];(1001,714),[224];(1001,717),[293];(1001,718),[322];(1001,721),[359];(1001,722),[390];(1001,732),[682];(1001,733),[691];(1001,734),[704];(1001,735),[742];(1001,736),[747];(1001,737),[894];(1001,738),[151;152];(1001,742),[217];(1001,743),[226];(1001,746),[295];(1001,747),[323;324];(1001,749),[360];(1001,750),[391;392];(1001,760),[684];(1001,761),[692];(1001,763),[705];(1001,764),[743];(1001,765),[748];(1001,766),[895];(1001,768),[154];(1001,772),[219];(1001,773),[228];(1001,776),[297];(1001,777),[326];(1001,780),[394];(1001,790),[686];(1001,797),[12];(1001,802),[528;529];(1002,147),[1014];(1002,371),[1377];(1003,78),[503];(1003,240),[1163];(1003,463),[676];(1003,509),[505];(1004,78),[503];(1004,240),[1162];(1004,463),[676];(1004,509),[505];(1005,513),[88];(1005,524),[90];(1005,535),[92];(1005,546),[94];(1005,557),[96];(1005,568),[97];(1005,612),[820];(1005,614),[822];(1005,615),[824];(1005,616),[826];(1005,617),[828];(1005,618),[830];(1006,78),[503];(1006,240),[1161];(1006,463),[676];(1006,509),[505];(1007,78),[503];(1007,240),[1160];(1007,463),[676];(1007,509),[505];(1008,94),[942];(1008,673),[65];(1008,684),[67];(1008,695),[68];(1009,72),[806];(1009,355),[1342];(1009,433),[1506];(1009,513),[88];(1009,524),[89];(1009,634),[169];(1009,737),[894];(1009,766),[895];(1010,58),[277];(1010,164),[1039];(1010,715),[233];(1010,744),[234];(1011,8),[581];(1011,10),[561];(1011,12),[268];(1011,32),[240];(1011,34),[387];(1011,67),[321];(1011,153),[1021];(1011,155),[1023];(1011,159),[1028];(1011,170),[1055];(1011,247),[1179];(1011,259),[1193];(1011,280),[1220];(1011,404),[1447];(1011,454),[886];(1011,455),[405];(1011,456),[370];(1011,459),[579];(1011,460),[602];(1011,471),[378];(1011,476),[389];(1011,478),[409];(1011,479),[413];(1011,480),[417];(1011,483),[419];(1011,484),[423];(1011,486),[425];(1011,488),[429];(1011,489),[433];(1011,491),[435];(1011,493),[437];(1011,512),[512];(1011,530),[576];(1011,531),[572];(1011,532),[574];(1011,533),[578];(1011,534),[583];(1011,536),[585];(1011,537),[587];(1011,538),[594];(1011,539),[596];(1011,541),[600];(1011,542),[604];(1011,544),[611];(1011,545),[609];(1011,547),[630];(1011,552),[621];(1011,585),[728];(1011,586),[726];(1011,587),[724];(1011,588),[730];(1011,622),[861];(1011,650),[214];(1011,652),[212];(1011,653),[223];(1011,654),[221];(1011,655),[229;230];(1011,656),[232];(1011,657),[239];(1011,658),[242];(1011,659),[245];(1011,660),[254;255];(1011,661),[249];(1011,663),[247];(1011,664),[253];(1011,665),[251];(1011,666),[258;259];(1011,667),[257];(1011,668),[261];(1011,669),[266;267];(1011,670),[265];(1011,671),[263];(1011,672),[270];(1011,674),[272];(1011,675),[276];(1011,676),[274];(1011,679),[291;292];(1011,681),[300];(1011,682),[306];(1011,683),[302];(1011,685),[304];(1011,686),[308];(1011,687),[309;310];(1011,688),[312];(1011,689),[318];(1011,690),[314];(1011,691),[316];(1011,692),[320];(1011,694),[330];(1011,696),[334];(1011,709),[150];(1011,713),[215];(1011,714),[224];(1011,717),[293];(1011,718),[322];(1011,722),[390];(1011,726),[544];(1011,732),[682];(1011,738),[152];(1011,742),[217];(1011,743),[226];(1011,746),[295];(1011,747),[324];(1011,750),[392];(1011,755),[545];(1011,760),[684];(1011,768),[154];(1011,772),[219];(1011,773),[228];(1011,776),[297];(1011,777),[326];(1011,780),[394];(1011,790),[686];(1011,797),[12];(1012,350),[1327];(1012,351),[1333];(1012,619),[856];(1012,620),[854];(1013,41),[523];(1013,515),[518];(1013,516),[519];(1014,453),[909];(1015,242),[1172];(1015,512),[511];(1016,78),[503];(1016,240),[1159];(1016,463),[676];(1016,509),[505];(1018,12),[268];(1018,32),[240];(1018,34),[387];(1018,54),[130];(1018,67),[321];(1018,110),[961];(1018,111),[962];(1018,153),[1021];(1018,155),[1023];(1018,159),[1028];(1018,170),[1055];(1018,296),[1249];(1018,328),[1295];(1018,425),[1489];(1018,454),[886];(1018,455),[405];(1018,456),[370];(1018,465),[114;115];(1018,466),[142];(1018,471),[378];(1018,476),[389];(1018,478),[409];(1018,479),[413];(1018,480),[417];(1018,481),[80];(1018,483),[419];(1018,484),[423];(1018,486),[425];(1018,488),[429];(1018,489),[433];(1018,491),[435];(1018,492),[73];(1018,493),[437];(1018,512),[512];(1018,522),[556];(1018,523),[558];(1018,525),[560];(1018,570),[679];(1018,571),[681];(1018,572),[688];(1018,573),[690];(1018,574),[697];(1018,575),[700];(1018,576),[702];(1018,577),[712];(1018,581),[716];(1018,585),[728];(1018,586),[726];(1018,587),[724];(1018,588),[730];(1018,589),[733];(1018,590),[735];(1018,594),[757];(1018,595),[764;765];(1018,596),[759];(1018,597),[761];(1018,598),[763];(1018,599),[767];(1018,603),[777];(1018,604),[779];(1018,609),[794];(1018,610),[796];(1018,611),[798];(1018,612),[820];(1018,614),[822];(1018,615),[824];(1018,616),[826];(1018,617),[828];(1018,618),[830];(1018,622),[861];(1018,624),[113];(1018,627),[117];(1018,628),[137];(1018,629),[48];(1018,630),[141];(1018,631),[146];(1018,633),[156];(1018,639),[179];(1018,640),[50];(1018,650),[214];(1018,651),[52];(1018,652),[212];(1018,653),[223];(1018,654),[221];(1018,655),[229;230];(1018,656),[232];(1018,657),[239];(1018,658),[242];(1018,659),[245];(1018,660),[254;255];(1018,661),[249];(1018,663),[247];(1018,664),[253];(1018,665),[251];(1018,666),[258;259];(1018,667),[257];(1018,668),[261];(1018,669),[266;267];(1018,670),[265];(1018,671),[263];(1018,672),[270];(1018,674),[272];(1018,675),[276];(1018,676),[274];(1018,679),[291;292];(1018,681),[300];(1018,682),[306];(1018,683),[302];(1018,685),[304];(1018,686),[308];(1018,687),[309;310];(1018,688),[312];(1018,689),[318];(1018,690),[314];(1018,691),[316];(1018,692),[320];(1018,694),[330];(1018,696),[334];(1018,708),[741];(1018,709),[150];(1018,710),[157];(1018,711),[163];(1018,713),[215];(1018,714),[224];(1018,717),[293];(1018,718),[322];(1018,722),[390];(1018,730),[74];(1018,732),[682];(1018,734),[704];(1018,735),[742];(1018,738),[152];(1018,739),[159];(1018,740),[165];(1018,742),[217];(1018,743),[226];(1018,746),[295];(1018,747),[324];(1018,750),[392];(1018,760),[684];(1018,762),[76];(1018,763),[706];(1018,764),[743;744];(1018,768),[154];(1018,769),[161];(1018,770),[167];(1018,772),[219];(1018,773),[228];(1018,776),[297];(1018,777),[326];(1018,780),[394];(1018,790),[686];(1018,792),[708];(1018,793),[746];(1018,795),[78];(1018,797),[12];(1019,12),[268];(1019,32),[240];(1019,34),[387];(1019,42),[516];(1019,67),[321];(1019,153),[1021];(1019,155),[1023];(1019,159),[1028];(1019,170),[1055];(1019,243),[1173];(1019,403),[1445];(1019,443),[1532];(1019,454),[886];(1019,455),[404;405];(1019,456),[370];(1019,471),[378];(1019,476),[389];(1019,478),[409];(1019,479),[413];(1019,480),[417];(1019,483),[419];(1019,484),[423];(1019,486),[425];(1019,488),[429];(1019,489),[433];(1019,491),[435];(1019,493),[437];(1019,512),[512];(1019,514),[521];(1019,585),[728];(1019,586),[726];(1019,587),[724];(1019,588),[730];(1019,622),[861];(1019,650),[214];(1019,652),[212];(1019,653),[223];(1019,654),[221];(1019,655),[229;230];(1019,656),[232];(1019,657),[239];(1019,658),[242];(1019,659),[245];(1019,660),[254;255];(1019,661),[249];(1019,663),[247];(1019,664),[253];(1019,665),[251];(1019,666),[258;259];(1019,667),[257];(1019,668),[261];(1019,669),[266;267];(1019,670),[265];(1019,671),[263];(1019,672),[270];(1019,674),[272];(1019,675),[276];(1019,676),[274];(1019,679),[291;292];(1019,681),[300];(1019,682),[306];(1019,683),[302];(1019,685),[304];(1019,686),[308];(1019,687),[309;310];(1019,688),[312];(1019,689),[318];(1019,690),[314];(1019,691),[316];(1019,692),[320];(1019,694),[330];(1019,696),[334];(1019,709),[150];(1019,713),[215];(1019,714),[224];(1019,717),[293];(1019,718),[322];(1019,722),[390];(1019,732),[682];(1019,738),[152];(1019,742),[217];(1019,743),[226];(1019,746),[295];(1019,747),[324];(1019,750),[392];(1019,760),[684];(1019,768),[154];(1019,772),[219];(1019,773),[228];(1019,776),[297];(1019,777),[326];(1019,780),[394];(1019,790),[686];(1019,797),[12];(1020,58),[277];(1020,164),[1038];(1020,715),[233];(1020,744),[234];(1021,1),[38];(1021,12),[268];(1021,32),[240];(1021,34),[387];(1021,54),[124];(1021,62),[46];(1021,63),[28];(1021,67),[321];(1021,110),[961];(1021,111),[962];(1021,153),[1021];(1021,155),[1023];(1021,159),[1028];(1021,170),[1055];(1021,296),[1249];(1021,302),[1256];(1021,328),[1295];(1021,425),[1489];(1021,450),[899];(1021,454),[886];(1021,455),[405];(1021,456),[370];(1021,464),[32];(1021,465),[114;115];(1021,466),[142];(1021,471),[378];(1021,476),[389];(1021,478),[409];(1021,479),[413];(1021,480),[417];(1021,481),[80];(1021,483),[419];(1021,484),[423];(1021,486),[425];(1021,488),[429];(1021,489),[433];(1021,491),[435];(1021,492),[73];(1021,493),[437];(1021,512),[512];(1021,522),[556];(1021,523),[558];(1021,525),[560];(1021,570),[679];(1021,571),[681];(1021,572),[688];(1021,573),[689;690];(1021,574),[697];(1021,575),[700];(1021,576),[702];(1021,577),[712];(1021,579),[31];(1021,581),[716];(1021,585),[728];(1021,586),[726];(1021,587),[724];(1021,588),[730];(1021,589),[733];(1021,590),[735];(1021,594),[757];(1021,595),[764;765];(1021,596),[759];(1021,597),[761];(1021,598),[763];(1021,599),[767];(1021,603),[777];(1021,604),[779];(1021,609),[794];(1021,610),[796];(1021,611),[798];(1021,612),[820];(1021,614),[822];(1021,615),[824];(1021,616),[826];(1021,617),[828];(1021,618),[830];(1021,622),[861];(1021,624),[113];(1021,627),[117];(1021,628),[137];(1021,629),[48];(1021,630),[141];(1021,631),[146];(1021,633),[156];(1021,639),[179];(1021,640),[50];(1021,650),[214];(1021,651),[52];(1021,652),[212];(1021,653),[223];(1021,654),[221];(1021,655),[229;230];(1021,656),[232];(1021,657),[239];(1021,658),[242];(1021,659),[245];(1021,660),[254;255];(1021,661),[249];(1021,663),[247];(1021,664),[253];(1021,665),[251];(1021,666),[258;259];(1021,667),[257];(1021,668),[261];(1021,669),[266;267];(1021,670),[265];(1021,671),[263];(1021,672),[270];(1021,674),[272];(1021,675),[276];(1021,676),[274];(1021,679),[291;292];(1021,681),[300];(1021,682),[306];(1021,683),[302];(1021,685),[304];(1021,686),[308];(1021,687),[309;310];(1021,688),[312];(1021,689),[318];(1021,690),[314];(1021,691),[316];(1021,692),[320];(1021,694),[330];(1021,696),[334];(1021,708),[741];(1021,709),[150];(1021,710),[157];(1021,711),[163];(1021,713),[215];(1021,714),[224];(1021,717),[293];(1021,718),[322];(1021,722),[390];(1021,730),[74];(1021,732),[682];(1021,734),[704];(1021,735),[742];(1021,738),[152];(1021,739),[159];(1021,740),[165];(1021,742),[217];(1021,743),[226];(1021,746),[295];(1021,747),[324];(1021,750),[392];(1021,760),[684];(1021,762),[76];(1021,763),[706];(1021,764),[743;744];(1021,768),[154];(1021,769),[161];(1021,770),[167];(1021,772),[219];(1021,773),[228];(1021,776),[297];(1021,777),[326];(1021,780),[394];(1021,790),[686];(1021,792),[708];(1021,793),[746];(1021,795),[78];(1021,797),[12];(1021,806),[29];(1022,380),[1395];(1024,12),[268];(1024,32),[240];(1024,34),[387];(1024,67),[321];(1024,153),[1021];(1024,155),[1023];(1024,159),[1028];(1024,170),[1055];(1024,181),[1069];(1024,381),[1398];(1024,382),[1400];(1024,436),[1514];(1024,437),[1518];(1024,446),[1538];(1024,454),[886];(1024,455),[405];(1024,456),[370];(1024,471),[378];(1024,476),[389];(1024,478),[409];(1024,479),[413];(1024,480),[417];(1024,483),[419];(1024,484),[423];(1024,486),[425];(1024,488),[429];(1024,489),[433];(1024,491),[435];(1024,493),[437];(1024,512),[512];(1024,585),[728];(1024,586),[726];(1024,587),[724];(1024,588),[730];(1024,622),[861];(1024,650),[214];(1024,652),[212];(1024,653),[223];(1024,654),[221];(1024,655),[229;230];(1024,656),[232];(1024,657),[239];(1024,658),[242];(1024,659),[245];(1024,660),[254;255];(1024,661),[249];(1024,663),[247];(1024,664),[253];(1024,665),[251];(1024,666),[258;259];(1024,667),[257];(1024,668),[261];(1024,669),[266;267];(1024,670),[265];(1024,671),[263];(1024,672),[270];(1024,674),[272];(1024,675),[276];(1024,676),[274];(1024,679),[291;292];(1024,681),[300];(1024,682),[306];(1024,683),[302];(1024,685),[304];(1024,686),[308];(1024,687),[309;310];(1024,688),[312];(1024,689),[318];(1024,690),[314];(1024,691),[316];(1024,692),[320];(1024,693),[328];(1024,694),[330];(1024,696),[334];(1024,709),[150];(1024,713),[215];(1024,714),[224];(1024,717),[293];(1024,718),[322];(1024,722),[390];(1024,732),[682];(1024,738),[152];(1024,742),[217];(1024,743),[226];(1024,746),[295];(1024,747),[323;324];(1024,750),[392];(1024,760),[684];(1024,768),[154];(1024,772),[219];(1024,773),[228];(1024,776),[297];(1024,777),[326];(1024,780),[394];(1024,790),[686];(1024,797),[12];(1025,10),[561];(1025,12),[268];(1025,32),[240];(1025,34),[387];(1025,67),[321];(1025,127),[987];(1025,153),[1021];(1025,155),[1023];(1025,159),[1028];(1025,170),[1055];(1025,247),[1179];(1025,404),[1447];(1025,454),[886];(1025,455),[405];(1025,456),[370];(1025,471),[378];(1025,476),[389];(1025,478),[409];(1025,479),[413];(1025,480),[417];(1025,483),[419];(1025,484),[423];(1025,486),[425];(1025,488),[429];(1025,489),[433];(1025,491),[435];(1025,493),[437];(1025,512),[512];(1025,530),[576];(1025,531),[572];(1025,532),[574];(1025,533),[577];(1025,585),[728];(1025,586),[726];(1025,587),[724];(1025,588),[730];(1025,622),[861];(1025,641),[185];(1025,642),[190;191];(1025,643),[187];(1025,644),[189];(1025,645),[192];(1025,650),[214];(1025,652),[212];(1025,653),[223];(1025,654),[221];(1025,655),[229;230];(1025,656),[232];(1025,657),[239];(1025,658),[242];(1025,659),[245];(1025,660),[254;255];(1025,661),[249];(1025,663),[247];(1025,664),[253];(1025,665),[251];(1025,666),[258;259];(1025,667),[257];(1025,668),[261];(1025,669),[266;267];(1025,670),[265];(1025,671),[263];(1025,672),[270];(1025,674),[272];(1025,675),[276];(1025,676),[274];(1025,679),[291;292];(1025,681),[300];(1025,682),[306];(1025,683),[302];(1025,685),[304];(1025,686),[308];(1025,687),[309;310];(1025,688),[312];(1025,689),[318];(1025,690),[314];(1025,691),[316];(1025,692),[320];(1025,694),[330];(1025,696),[334];(1025,709),[150];(1025,713),[215];(1025,714),[224];(1025,717),[293];(1025,718),[322];(1025,722),[390];(1025,726),[544];(1025,732),[682];(1025,738),[152];(1025,742),[217];(1025,743),[226];(1025,746),[295];(1025,747),[324];(1025,750),[392];(1025,755),[545];(1025,760),[684];(1025,768),[154];(1025,772),[219];(1025,773),[228];(1025,776),[297];(1025,777),[326];(1025,780),[394];(1025,790),[686];(1025,797),[12];(1026,78),[503];(1026,240),[1158];(1026,463),[676];(1026,509),[505];(1027,12),[268];(1027,32),[240];(1027,34),[387];(1027,67),[321];(1027,153),[1021];(1027,155),[1023];(1027,159),[1028];(1027,170),[1055];(1027,241),[1170];(1027,454),[886];(1027,455),[405];(1027,456),[370];(1027,471),[378];(1027,476),[389];(1027,478),[409];(1027,479),[413];(1027,480),[417];(1027,483),[419];(1027,484),[423];(1027,486),[425];(1027,488),[429];(1027,489),[433];(1027,491),[435];(1027,493),[437];(1027,511),[509];(1027,512),[512];(1027,585),[728];(1027,586),[726];(1027,587),[724];(1027,588),[730];(1027,622),[861];(1027,650),[214];(1027,652),[212];(1027,653),[223];(1027,654),[221];(1027,655),[229;230];(1027,656),[232];(1027,657),[239];(1027,658),[242];(1027,659),[245];(1027,660),[254;255];(1027,661),[249];(1027,663),[247];(1027,664),[253];(1027,665),[251];(1027,666),[258;259];(1027,667),[257];(1027,668),[261];(1027,669),[266;267];(1027,670),[265];(1027,671),[263];(1027,672),[270];(1027,674),[272];(1027,675),[276];(1027,676),[274];(1027,679),[291;292];(1027,681),[300];(1027,682),[306];(1027,683),[302];(1027,685),[304];(1027,686),[308];(1027,687),[309;310];(1027,688),[312];(1027,689),[318];(1027,690),[314];(1027,691),[316];(1027,692),[320];(1027,694),[330];(1027,696),[334];(1027,709),[150];(1027,713),[215];(1027,714),[224];(1027,717),[293];(1027,718),[322];(1027,722),[390];(1027,732),[682];(1027,738),[152];(1027,742),[217];(1027,743),[226];(1027,746),[295];(1027,747),[324];(1027,750),[392];(1027,760),[684];(1027,768),[154];(1027,772),[219];(1027,773),[228];(1027,776),[297];(1027,777),[326];(1027,780),[394];(1027,790),[686];(1027,797),[12];(1029,12),[268];(1029,24),[703];(1029,32),[240];(1029,34),[387];(1029,43),[514;515];(1029,51),[373;374;375;376];(1029,61),[347];(1029,66),[364];(1029,67),[321];(1029,73),[869];(1029,79),[399];(1029,153),[1021];(1029,155),[1023];(1029,159),[1028];(1029,170),[1055];(1029,181),[1069];(1029,189),[1082];(1029,193),[1091];(1029,195),[1094];(1029,204),[1105];(1029,303),[1258];(1029,308),[1266];(1029,309),[1268];(1029,321),[1286];(1029,328),[1295];(1029,355),[1341];(1029,381),[1397];(1029,382),[1400];(1029,385),[1407];(1029,387),[1411];(1029,419),[1479];(1029,420),[1482];(1029,421),[1484];(1029,424),[1488];(1029,425),[1491];(1029,433),[1505];(1029,437),[1517];(1029,444),[1534];(1029,454),[886];(1029,455),[405];(1029,456),[370];(1029,471),[378];(1029,476),[389];(1029,478),[409];(1029,479),[413];(1029,480),[417];(1029,483),[419];(1029,484),[423];(1029,486),[425];(1029,488),[429];(1029,489),[433];(1029,491),[435];(1029,493),[437];(1029,512),[512];(1029,517),[532];(1029,543),[606];(1029,555),[629];(1029,564),[660];(1029,577),[712];(1029,585),[728];(1029,586),[726];(1029,587),[724];(1029,588),[730];(1029,594),[757];(1029,595),[764];(1029,596),[759];(1029,600),[769];(1029,622),[861];(1029,635),[171];(1029,636),[173];(1029,637),[175];(1029,650),[214];(1029,652),[212];(1029,653),[223];(1029,654),[221];(1029,655),[229;230];(1029,656),[232];(1029,657),[239];(1029,658),[242];(1029,659),[245];(1029,660),[254;255];(1029,661),[249];(1029,663),[247];(1029,664),[253];(1029,665),[251];(1029,666),[258;259];(1029,667),[257];(1029,668),[261];(1029,669),[266;267];(1029,670),[265];(1029,671),[263];(1029,672),[270];(1029,674),[272];(1029,675),[276];(1029,676),[274];(1029,679),[291;292];(1029,681),[300];(1029,682),[306];(1029,683),[302];(1029,685),[304];(1029,686),[308];(1029,687),[309;310];(1029,688),[312];(1029,689),[318];(1029,690),[314];(1029,691),[316];(1029,692),[320];(1029,694),[330];(1029,696),[334];(1029,698),[338];(1029,700),[348];(1029,704),[366];(1029,705),[368];(1029,708),[741];(1029,709),[150];(1029,713),[215];(1029,714),[224];(1029,717),[293];(1029,718),[322];(1029,721),[359];(1029,722),[390];(1029,732),[682];(1029,733),[691];(1029,734),[704];(1029,735),[742];(1029,736),[747];(1029,737),[894];(1029,738),[151;152];(1029,742),[217];(1029,743),[226];(1029,746),[295];(1029,747),[323;324];(1029,749),[360];(1029,750),[391;392];(1029,760),[684];(1029,761),[692];(1029,763),[705];(1029,764),[743];(1029,765),[748];(1029,766),[895];(1029,768),[154];(1029,772),[219];(1029,773),[228];(1029,776),[297];(1029,777),[326];(1029,780),[394];(1029,790),[686];(1029,797),[12];(1029,802),[528;529];(1030,123),[982];(1031,92),[934];(1031,673),[64];(1032,94),[941];(1032,673),[65];(1032,684),[67];(1032,695),[68];(1033,12),[268];(1033,24),[703];(1033,32),[240];(1033,34),[387];(1033,43),[514;515];(1033,51),[373;374;375;376];(1033,54),[134];(1033,57),[288];(1033,59),[298];(1033,66),[364];(1033,67),[321];(1033,68),[203];(1033,73),[874];(1033,79),[399];(1033,110),[961];(1033,111),[962];(1033,133),[997];(1033,153),[1021];(1033,155),[1023];(1033,159),[1028];(1033,169),[1054];(1033,170),[1055];(1033,181),[1069];(1033,188),[1079];(1033,190),[1086];(1033,193),[1091];(1033,195),[1094];(1033,204),[1105];(1033,296),[1249];(1033,303),[1258];(1033,308),[1266];(1033,309),[1268];(1033,321),[1286];(1033,328),[1295];(1033,355),[1341];(1033,381),[1397];(1033,382),[1400];(1033,385),[1407];(1033,387),[1411];(1033,419),[1479];(1033,420),[1482];(1033,421),[1484];(1033,424),[1488];(1033,425),[1489;1491];(1033,433),[1505];(1033,437),[1517];(1033,444),[1534];(1033,454),[886];(1033,455),[405];(1033,456),[370];(1033,465),[114;115];(1033,466),[142];(1033,468),[350];(1033,471),[378];(1033,476),[389];(1033,478),[409];(1033,479),[413];(1033,480),[417];(1033,481),[80];(1033,483),[419];(1033,484),[423];(1033,486),[425];(1033,488),[429];(1033,489),[433];(1033,491),[435];(1033,492),[73];(1033,493),[437];(1033,512),[512];(1033,517),[532];(1033,522),[556];(1033,523),[558];(1033,525),[560];(1033,543),[606];(1033,555),[629];(1033,564),[660];(1033,570),[679];(1033,571),[681];(1033,572),[688];(1033,573),[690];(1033,574),[697];(1033,575),[700];(1033,576),[702];(1033,577),[712];(1033,581),[716];(1033,585),[728];(1033,586),[726];(1033,587),[724];(1033,588),[730];(1033,589),[733];(1033,590),[735];(1033,594),[757];(1033,595),[764;765];(1033,596),[759];(1033,597),[761];(1033,598),[763];(1033,599),[767];(1033,600),[769];(1033,603),[777];(1033,604),[779];(1033,609),[794];(1033,610),[796];(1033,611),[798];(1033,612),[820];(1033,614),[822];(1033,615),[824];(1033,616),[826];(1033,617),[828];(1033,618),[830];(1033,622),[861];(1033,624),[113];(1033,627),[117];(1033,628),[137];(1033,629),[48];(1033,630),[141];(1033,631),[146];(1033,633),[156];(1033,635),[171];(1033,636),[173];(1033,637),[175];(1033,639),[179];(1033,640),[50];(1033,649),[205];(1033,650),[214];(1033,651),[52];(1033,652),[212];(1033,653),[223];(1033,654),[221];(1033,655),[229;230];(1033,656),[232];(1033,657),[239];(1033,658),[242];(1033,659),[245];(1033,660),[254;255];(1033,661),[249];(1033,663),[247];(1033,664),[253];(1033,665),[251];(1033,666),[258;259];(1033,667),[257];(1033,668),[261];(1033,669),[266;267];(1033,670),[265];(1033,671),[263];(1033,672),[270];(1033,674),[272];(1033,675),[276];(1033,676),[274];(1033,678),[287];(1033,679),[291;292];(1033,680),[290];(1033,681),[300];(1033,682),[306];(1033,683),[302];(1033,685),[304];(1033,686),[308];(1033,687),[309;310];(1033,688),[312];(1033,689),[318];(1033,690),[314];(1033,691),[316];(1033,692),[320];(1033,694),[330];(1033,696),[334];(1033,698),[338];(1033,699),[345];(1033,700),[348];(1033,701),[352];(1033,704),[366];(1033,705),[368];(1033,708),[741];(1033,709),[150];(1033,710),[157];(1033,711),[163];(1033,712),[206];(1033,713),[215];(1033,714),[224];(1033,717),[293];(1033,718),[322];(1033,720),[340];(1033,721),[359];(1033,722),[390];(1033,730),[74];(1033,732),[682];(1033,733),[691];(1033,734),[704];(1033,735),[742];(1033,736),[747];(1033,737),[894];(1033,738),[151;152];(1033,739),[159];(1033,740),[165];(1033,741),[208];(1033,742),[217];(1033,743),[226];(1033,746),[295];(1033,747),[323;324];(1033,748),[341];(1033,749),[360];(1033,750),[391;392];(1033,760),[684];(1033,761),[692];(1033,762),[76];(1033,763),[705;706];(1033,764),[743;744];(1033,765),[748];(1033,766),[895];(1033,768),[154];(1033,769),[161];(1033,770),[167];(1033,771),[210];(1033,772),[219];(1033,773),[228];(1033,776),[297];(1033,777),[326];(1033,780),[394];(1033,790),[686];(1033,792),[708];(1033,793),[746];(1033,795),[78];(1033,797),[12];(1033,802),[528;529];(1034,344),[1319];(1035,78),[503];(1035,240),[1157];(1035,463),[676];(1035,509),[505];(1036,1),[44];(1036,12),[268];(1036,23),[662;663];(1036,32),[240];(1036,34),[387];(1036,54),[133];(1036,63),[28];(1036,67),[321];(1036,69),[56];(1036,70),[53;54];(1036,110),[961];(1036,111),[962];(1036,153),[1021];(1036,155),[1023];(1036,159),[1028];(1036,170),[1055];(1036,296),[1249];(1036,302),[1256];(1036,328),[1295];(1036,425),[1489];(1036,450),[899];(1036,454),[886];(1036,455),[405];(1036,456),[370];(1036,464),[32];(1036,465),[114;115];(1036,466),[142];(1036,471),[378];(1036,476),[389];(1036,478),[409];(1036,479),[413];(1036,480),[417];(1036,481),[80];(1036,483),[419];(1036,484),[423];(1036,486),[425];(1036,488),[429];(1036,489),[433];(1036,491),[435];(1036,492),[73];(1036,493),[437];(1036,512),[512];(1036,522),[556];(1036,523),[558];(1036,525),[560];(1036,570),[679];(1036,571),[681];(1036,572),[688];(1036,573),[689;690];(1036,574),[697];(1036,575),[700];(1036,576),[702];(1036,577),[712];(1036,579),[31];(1036,581),[716];(1036,585),[728];(1036,586),[726];(1036,587),[724];(1036,588),[730];(1036,589),[733];(1036,590),[735];(1036,594),[757];(1036,595),[764;765];(1036,596),[759];(1036,597),[761];(1036,598),[763];(1036,599),[767];(1036,603),[777];(1036,604),[779];(1036,609),[794];(1036,610),[796];(1036,611),[798];(1036,612),[820];(1036,614),[822];(1036,615),[824];(1036,616),[826];(1036,617),[828];(1036,618),[830];(1036,622),[861];(1036,624),[113];(1036,627),[117];(1036,628),[137];(1036,629),[48];(1036,630),[141];(1036,631),[146];(1036,633),[156];(1036,639),[179];(1036,640),[50];(1036,650),[214];(1036,651),[52];(1036,652),[212];(1036,653),[223];(1036,654),[221];(1036,655),[229;230];(1036,656),[232];(1036,657),[239];(1036,658),[242];(1036,659),[245];(1036,660),[254;255];(1036,661),[249];(1036,663),[247];(1036,664),[253];(1036,665),[251];(1036,666),[258;259];(1036,667),[257];(1036,668),[261];(1036,669),[266;267];(1036,670),[265];(1036,671),[263];(1036,672),[270];(1036,674),[272];(1036,675),[276];(1036,676),[274];(1036,679),[291;292];(1036,681),[300];(1036,682),[306];(1036,683),[302];(1036,685),[304];(1036,686),[308];(1036,687),[309;310];(1036,688),[312];(1036,689),[318];(1036,690),[314];(1036,691),[316];(1036,692),[320];(1036,694),[330];(1036,696),[334];(1036,708),[741];(1036,709),[150];(1036,710),[157];(1036,711),[163];(1036,713),[215];(1036,714),[224];(1036,717),[293];(1036,718),[322];(1036,722),[390];(1036,730),[74];(1036,732),[682];(1036,734),[704];(1036,735),[742];(1036,738),[152];(1036,739),[159];(1036,740),[165];(1036,742),[217];(1036,743),[226];(1036,746),[295];(1036,747),[324];(1036,750),[392];(1036,760),[684];(1036,762),[76];(1036,763),[706];(1036,764),[743;744];(1036,768),[154];(1036,769),[161];(1036,770),[167];(1036,772),[219];(1036,773),[228];(1036,776),[297];(1036,777),[326];(1036,780),[394];(1036,790),[686];(1036,792),[708];(1036,793),[746];(1036,795),[78];(1036,797),[12];(1036,806),[29];(1037,118),[971];(1037,362),[1356];(1037,710),[157];(1037,739),[158];(1038,453),[908];(1039,453),[907];(1040,453),[906];(1041,376),[1389];(1042,393),[1423];(1043,393),[1422];(1044,393),[1421];(1045,72),[815];(1045,355),[1342];(1045,433),[1506];(1045,513),[88];(1045,524),[89];(1045,634),[169];(1045,737),[894];(1045,766),[895];(1046,72),[812];(1046,355),[1342];(1046,433),[1506];(1046,513),[88];(1046,524),[89];(1046,634),[169];(1046,737),[894];(1046,766),[895];(1047,197),[1097];(1048,72),[800];(1048,355),[1342];(1048,433),[1506];(1048,513),[88];(1048,524),[89];(1048,634),[169];(1048,737),[894];(1048,766),[895];(1049,1),[44];(1049,5),[893];(1049,7),[196];(1049,9),[183];(1049,10),[562];(1049,12),[268];(1049,13),[283];(1049,24),[703];(1049,29),[739];(1049,30),[738];(1049,32),[240];(1049,34),[387];(1049,35),[9];(1049,37),[6];(1049,43),[514;515];(1049,44),[717];(1049,51),[373;374;375;376];(1049,54),[132;133];(1049,63),[28];(1049,66),[364];(1049,67),[321];(1049,69),[55];(1049,70),[53];(1049,73),[875;876];(1049,79),[398;399;400];(1049,89),[930];(1049,99),[948];(1049,100),[949];(1049,101),[950];(1049,102),[951];(1049,106),[957];(1049,110),[961];(1049,111),[962];(1049,120),[975;976];(1049,122),[980];(1049,127),[987];(1049,128),[989;990];(1049,138),[1003];(1049,153),[1021];(1049,155),[1023];(1049,159),[1028];(1049,167),[1050;1051];(1049,170),[1055];(1049,181),[1068;1069;1070];(1049,183),[1073];(1049,185),[1075];(1049,190),[1085];(1049,193),[1091];(1049,195),[1094];(1049,204),[1105];(1049,206),[1108];(1049,208),[1110];(1049,211),[1113];(1049,215),[1118];(1049,216),[1119];(1049,223),[1128];(1049,227),[1134];(1049,247),[1179];(1049,295),[1248];(1049,296),[1249];(1049,302),[1256];(1049,303),[1258];(1049,308),[1266;1267];(1049,309),[1268];(1049,316),[1278];(1049,319),[1284];(1049,321),[1286];(1049,328),[1295];(1049,344),[1317];(1049,355),[1341];(1049,363),[1359;1360];(1049,366),[1366];(1049,367),[1367];(1049,381),[1397;1398];(1049,382),[1399;1400;1401];(1049,384),[1405;1406];(1049,385),[1407];(1049,387),[1411];(1049,392),[1420];(1049,404),[1447];(1049,419),[1479];(1049,420),[1482;1483];(1049,421),[1484];(1049,422),[1486];(1049,423),[1487];(1049,424),[1488];(1049,425),[1489;1491];(1049,433),[1505];(1049,434),[1508];(1049,436),[1516];(1049,437),[1517;1518];(1049,438),[1519];(1049,444),[1534;1535];(1049,445),[1537];(1049,446),[1540];(1049,447),[16];(1049,448),[23];(1049,450),[899];(1049,454),[886];(1049,455),[405];(1049,456),[370];(1049,464),[32];(1049,465),[114;115];(1049,466),[142];(1049,467),[181];(1049,468),[350];(1049,471),[378];(1049,476),[389];(1049,477),[407];(1049,478),[409];(1049,479),[413];(1049,480),[416;417];(1049,481),[80];(1049,482),[415];(1049,483),[419];(1049,484),[422;423];(1049,485),[421];(1049,486),[425];(1049,487),[427];(1049,488),[429];(1049,489),[432;433];(1049,490),[431];(1049,491),[435];(1049,492),[73];(1049,493),[437];(1049,494),[444;445];(1049,495),[446;447];(1049,503),[82;83];(1049,512),[512];(1049,513),[87];(1049,517),[532];(1049,522),[556];(1049,523),[558];(1049,525),[560];(1049,543),[606];(1049,555),[629];(1049,564),[660];(1049,566),[673];(1049,567),[671];(1049,569),[675];(1049,570),[679];(1049,571),[681];(1049,572),[688];(1049,573),[689;690];(1049,574),[697];(1049,575),[700];(1049,576),[702];(1049,577),[712];(1049,579),[31];(1049,580),[102;103];(1049,581),[716];(1049,583),[721];(1049,585),[727;728];(1049,586),[726];(1049,587),[724];(1049,588),[730];(1049,589),[733];(1049,590),[735];(1049,592),[753];(1049,593),[755];(1049,594),[757];(1049,595),[764;765];(1049,596),[759];(1049,597),[761];(1049,598),[763];(1049,599),[767];(1049,600),[769];(1049,603),[777];(1049,604),[779];(1049,609),[794];(1049,610),[796];(1049,611),[798];(1049,612),[820];(1049,614),[822];(1049,615),[824];(1049,616),[826];(1049,617),[828];(1049,618),[830];(1049,622),[861];(1049,624),[113];(1049,627),[117];(1049,628),[137];(1049,629),[48];(1049,630),[141];(1049,631),[146];(1049,633),[156];(1049,635),[171];(1049,636),[173];(1049,637),[175];(1049,639),[179];(1049,640),[50];(1049,641),[185];(1049,642),[190;191];(1049,643),[187];(1049,644),[189];(1049,645),[193];(1049,646),[194;195];(1049,647),[198];(1049,648),[200];(1049,650),[214];(1049,651),[52];(1049,652),[212];(1049,653),[223];(1049,654),[221];(1049,655),[229;230];(1049,656),[232];(1049,657),[239];(1049,658),[242];(1049,659),[245];(1049,660),[254;255];(1049,661),[249];(1049,663),[247];(1049,664),[253];(1049,665),[251];(1049,666),[258;259];(1049,667),[257];(1049,668),[261];(1049,669),[266;267];(1049,670),[265];(1049,671),[263];(1049,672),[270];(1049,674),[272];(1049,675),[276];(1049,676),[274];(1049,679),[291;292];(1049,681),[300];(1049,682),[306];(1049,683),[302];(1049,685),[304];(1049,686),[308];(1049,687),[309;310];(1049,688),[312];(1049,689),[318];(1049,690),[314];(1049,691),[316];(1049,692),[320];(1049,693),[327;328];(1049,694),[330];(1049,696),[333;334];(1049,697),[332];(1049,698),[338];(1049,700),[348];(1049,701),[352];(1049,704),[366];(1049,705),[368];(1049,706),[8];(1049,708),[741];(1049,709),[150];(1049,710),[157];(1049,711),[163];(1049,712),[206];(1049,713),[215];(1049,714),[224];(1049,717),[293];(1049,718),[322];(1049,721),[359];(1049,722),[390];(1049,723),[466];(1049,726),[544];(1049,730),[74];(1049,732),[682];(1049,733),[691];(1049,734),[704];(1049,735),[742];(1049,736),[747];(1049,737),[894];(1049,738),[151;152];(1049,739),[159];(1049,740),[164;165];(1049,741),[207];(1049,742),[216;217];(1049,743),[226];(1049,746),[295];(1049,747),[323;324];(1049,749),[360];(1049,750),[391;392];(1049,752),[467];(1049,755),[545];(1049,760),[684];(1049,761),[692];(1049,762),[75;76];(1049,763),[705;706];(1049,764),[743;744];(1049,765),[748];(1049,766),[895];(1049,767),[10];(1049,768),[154];(1049,769),[161];(1049,770),[167];(1049,772),[219];(1049,773),[228];(1049,776),[297];(1049,777),[326];(1049,780),[394];(1049,790),[686];(1049,792),[708];(1049,793),[746];(1049,795),[78];(1049,797),[12];(1049,798),[84;85];(1049,802),[528;529];(1049,806),[29];(1050,93),[939];(1050,635),[171];(1050,636),[172];(1050,673),[65];(1050,684),[66];(1051,453),[905];(1052,39),[666];(1052,294),[1243];(1052,728),[588];(1052,757),[589];(1053,12),[268];(1053,24),[703];(1053,32),[240];(1053,34),[387];(1053,43),[514;515];(1053,51),[373;374;375;376];(1053,65),[335];(1053,66),[364];(1053,67),[321];(1053,73),[878];(1053,79),[399];(1053,142),[1007];(1053,153),[1021];(1053,155),[1023];(1053,159),[1028];(1053,170),[1055];(1053,181),[1069];(1053,193),[1091];(1053,195),[1094];(1053,204),[1105];(1053,303),[1258];(1053,308),[1266];(1053,309),[1268];(1053,321),[1286];(1053,328),[1295];(1053,355),[1341];(1053,368),[1368];(1053,369),[1373];(1053,381),[1397];(1053,382),[1400];(1053,385),[1407];(1053,387),[1411];(1053,419),[1479];(1053,420),[1482];(1053,421),[1484];(1053,424),[1488];(1053,425),[1491];(1053,433),[1505];(1053,435),[1509];(1053,437),[1517];(1053,444),[1534];(1053,454),[886];(1053,455),[405];(1053,456),[370];(1053,471),[378];(1053,476),[389];(1053,478),[409];(1053,479),[413];(1053,480),[417];(1053,483),[419];(1053,484),[423];(1053,486),[425];(1053,488),[429];(1053,489),[433];(1053,491),[435];(1053,493),[437];(1053,512),[512];(1053,517),[532];(1053,543),[606];(1053,555),[629];(1053,564),[660];(1053,577),[712];(1053,585),[728];(1053,586),[726];(1053,587),[724];(1053,588),[730];(1053,594),[757];(1053,595),[764];(1053,596),[759];(1053,600),[769];(1053,622),[861];(1053,635),[171];(1053,636),[173];(1053,637),[175];(1053,650),[214];(1053,652),[212];(1053,653),[223];(1053,654),[221];(1053,655),[229;230];(1053,656),[232];(1053,657),[239];(1053,658),[242];(1053,659),[245];(1053,660),[254;255];(1053,661),[249];(1053,663),[247];(1053,664),[253];(1053,665),[251];(1053,666),[258;259];(1053,667),[257];(1053,668),[261];(1053,669),[266;267];(1053,670),[265];(1053,671),[263];(1053,672),[270];(1053,674),[272];(1053,675),[276];(1053,676),[274];(1053,679),[291;292];(1053,681),[300];(1053,682),[306];(1053,683),[302];(1053,685),[304];(1053,686),[308];(1053,687),[309;310];(1053,688),[312];(1053,689),[318];(1053,690),[314];(1053,691),[316];(1053,692),[320];(1053,694),[330];(1053,696),[334];(1053,698),[338];(1053,700),[348];(1053,704),[366];(1053,705),[368];(1053,708),[741];(1053,709),[150];(1053,713),[215];(1053,714),[224];(1053,717),[293];(1053,718),[322];(1053,721),[359];(1053,722),[390];(1053,732),[682];(1053,733),[691];(1053,734),[704];(1053,735),[742];(1053,736),[747];(1053,737),[894];(1053,738),[151;152];(1053,742),[217];(1053,743),[225;226];(1053,746),[295];(1053,747),[323;324];(1053,749),[360];(1053,750),[391;392];(1053,760),[684];(1053,761),[692];(1053,763),[705];(1053,764),[743];(1053,765),[748];(1053,766),[895];(1053,768),[154];(1053,772),[219];(1053,773),[228];(1053,776),[297];(1053,777),[326];(1053,780),[394];(1053,790),[686];(1053,797),[12];(1053,802),[528;529];(1054,72),[811];(1054,355),[1342];(1054,433),[1506];(1054,513),[88];(1054,524),[89];(1054,634),[169];(1054,737),[894];(1054,766),[895];(1055,510),[507];(1056,80),[180];(1056,638),[177];(1057,81),[506];(1057,478),[408];(1057,797),[12];(1058,78),[503];(1058,240),[1156];(1058,463),[676];(1058,509),[505];(1059,78),[503];(1059,240),[1155];(1059,463),[676];(1059,509),[505];(1060,251),[1183];(1060,522),[556];(1060,523),[557];(1061,12),[268];(1061,32),[240];(1061,34),[387];(1061,67),[321];(1061,153),[1021];(1061,155),[1023];(1061,159),[1028];(1061,170),[1055];(1061,454),[886];(1061,455),[405];(1061,456),[370];(1061,471),[378];(1061,476),[389];(1061,478),[409];(1061,479),[413];(1061,480),[417];(1061,483),[419];(1061,484),[423];(1061,486),[425];(1061,488),[429];(1061,489),[433];(1061,491),[435];(1061,493),[437];(1061,512),[512];(1061,585),[728];(1061,586),[726];(1061,587),[724];(1061,588),[730];(1061,622),[861];(1061,650),[214];(1061,652),[212];(1061,653),[223];(1061,654),[221];(1061,655),[229;230];(1061,656),[232];(1061,657),[239];(1061,658),[242];(1061,659),[245];(1061,660),[254;255];(1061,661),[249];(1061,663),[247];(1061,664),[253];(1061,665),[251];(1061,666),[258;259];(1061,667),[257];(1061,668),[261];(1061,669),[266;267];(1061,670),[265];(1061,671),[263];(1061,672),[270];(1061,674),[272];(1061,675),[276];(1061,676),[274];(1061,679),[291;292];(1061,681),[300];(1061,682),[306];(1061,683),[302];(1061,685),[304];(1061,686),[308];(1061,687),[309;310];(1061,688),[312];(1061,689),[318];(1061,690),[314];(1061,691),[316];(1061,692),[320];(1061,694),[330];(1061,696),[334];(1061,709),[150];(1061,713),[215];(1061,714),[224];(1061,717),[293];(1061,718),[322];(1061,722),[390];(1061,732),[682];(1061,738),[152];(1061,742),[217];(1061,743),[226];(1061,746),[295];(1061,747),[324];(1061,750),[392];(1061,760),[684];(1061,768),[154];(1061,772),[219];(1061,773),[228];(1061,776),[297];(1061,777),[326];(1061,780),[394];(1061,790),[686];(1061,797),[12];(1062,232),[1143];(1064,357),[1345];(1065,72),[809];(1065,355),[1342];(1065,433),[1506];(1065,513),[88];(1065,524),[89];(1065,634),[169];(1065,737),[894];(1065,766),[895];(1066,175),[1062];(1066,298),[1251];(1066,566),[672];(1066,681),[300];(1066,682),[305];(1067,314),[1276];(1067,317),[1280];(1067,451),[901];(1067,629),[47];(1068,314),[1275];(1068,317),[1279];(1068,451),[900];(1068,629),[47];(1069,1),[44];(1069,5),[893];(1069,7),[196];(1069,9),[183];(1069,10),[562];(1069,12),[268];(1069,13),[283];(1069,24),[703];(1069,29),[739];(1069,30),[738];(1069,32),[240];(1069,34),[387];(1069,35),[9];(1069,37),[6];(1069,43),[514;515];(1069,44),[717];(1069,51),[373;374;375;376];(1069,54),[132;133];(1069,63),[28];(1069,66),[364];(1069,67),[321];(1069,69),[55];(1069,70),[53];(1069,73),[863;875;876];(1069,79),[398;399;400];(1069,89),[930];(1069,99),[948];(1069,100),[949];(1069,101),[950];(1069,102),[951];(1069,106),[957];(1069,110),[961];(1069,111),[962];(1069,120),[975;976];(1069,122),[980];(1069,127),[987];(1069,128),[989;990];(1069,138),[1003];(1069,153),[1021];(1069,155),[1023];(1069,159),[1028];(1069,167),[1050;1051];(1069,170),[1055];(1069,181),[1068;1069;1070];(1069,183),[1073];(1069,185),[1075];(1069,190),[1085];(1069,193),[1091];(1069,195),[1094];(1069,204),[1105];(1069,206),[1108];(1069,208),[1110];(1069,211),[1113];(1069,215),[1118];(1069,216),[1119];(1069,223),[1128];(1069,227),[1134];(1069,247),[1179];(1069,295),[1248];(1069,296),[1249];(1069,302),[1256];(1069,303),[1258];(1069,308),[1266;1267];(1069,309),[1268];(1069,316),[1278];(1069,319),[1284];(1069,321),[1286];(1069,328),[1295];(1069,344),[1317];(1069,355),[1341];(1069,363),[1359;1360];(1069,366),[1366];(1069,367),[1367];(1069,381),[1397;1398];(1069,382),[1399;1400;1401];(1069,384),[1405;1406];(1069,385),[1407];(1069,387),[1411];(1069,392),[1420];(1069,404),[1447];(1069,419),[1479];(1069,420),[1482;1483];(1069,421),[1484];(1069,422),[1486];(1069,423),[1487];(1069,424),[1488];(1069,425),[1489;1491];(1069,433),[1505];(1069,434),[1508];(1069,436),[1516];(1069,437),[1517;1518];(1069,438),[1519];(1069,444),[1534;1535];(1069,445),[1537];(1069,446),[1540];(1069,447),[18];(1069,448),[25];(1069,450),[899];(1069,454),[886];(1069,455),[405];(1069,456),[370];(1069,464),[32];(1069,465),[114;115];(1069,466),[142];(1069,467),[181];(1069,468),[350];(1069,471),[378];(1069,476),[389];(1069,477),[407];(1069,478),[409];(1069,479),[413];(1069,480),[416;417];(1069,481),[80];(1069,482),[415];(1069,483),[419];(1069,484),[422;423];(1069,485),[421];(1069,486),[425];(1069,487),[427];(1069,488),[429];(1069,489),[432;433];(1069,490),[431];(1069,491),[435];(1069,492),[73];(1069,493),[437];(1069,494),[444;445];(1069,495),[446;447];(1069,503),[82;83];(1069,512),[512];(1069,513),[87];(1069,517),[532];(1069,522),[556];(1069,523),[558];(1069,525),[560];(1069,543),[606];(1069,555),[629];(1069,564),[660];(1069,566),[673];(1069,567),[671];(1069,569),[675];(1069,570),[679];(1069,571),[681];(1069,572),[688];(1069,573),[689;690];(1069,574),[697];(1069,575),[700];(1069,576),[702];(1069,577),[712];(1069,579),[31];(1069,580),[102;103];(1069,581),[716];(1069,583),[721];(1069,585),[727;728];(1069,586),[726];(1069,587),[724];(1069,588),[730];(1069,589),[733];(1069,590),[735];(1069,592),[753];(1069,593),[755];(1069,594),[757];(1069,595),[764;765];(1069,596),[759];(1069,597),[761];(1069,598),[763];(1069,599),[767];(1069,600),[769];(1069,603),[777];(1069,604),[779];(1069,609),[794];(1069,610),[796];(1069,611),[798];(1069,612),[820];(1069,614),[822];(1069,615),[824];(1069,616),[826];(1069,617),[828];(1069,618),[830];(1069,622),[861];(1069,624),[113];(1069,627),[117];(1069,628),[137];(1069,629),[48];(1069,630),[141];(1069,631),[146];(1069,633),[156];(1069,635),[171];(1069,636),[173];(1069,637),[175];(1069,639),[179];(1069,640),[50];(1069,641),[185];(1069,642),[190;191];(1069,643),[187];(1069,644),[189];(1069,645),[193];(1069,646),[194;195];(1069,647),[198];(1069,648),[200];(1069,650),[214];(1069,651),[52];(1069,652),[212];(1069,653),[223];(1069,654),[221];(1069,655),[229;230];(1069,656),[232];(1069,657),[239];(1069,658),[242];(1069,659),[245];(1069,660),[254;255];(1069,661),[249];(1069,663),[247];(1069,664),[253];(1069,665),[251];(1069,666),[258;259];(1069,667),[257];(1069,668),[261];(1069,669),[266;267];(1069,670),[265];(1069,671),[263];(1069,672),[270];(1069,674),[272];(1069,675),[276];(1069,676),[274];(1069,679),[291;292];(1069,681),[300];(1069,682),[306];(1069,683),[302];(1069,685),[304];(1069,686),[308];(1069,687),[309;310];(1069,688),[312];(1069,689),[318];(1069,690),[314];(1069,691),[316];(1069,692),[320];(1069,693),[327;328];(1069,694),[330];(1069,696),[333;334];(1069,697),[332];(1069,698),[338];(1069,700),[348];(1069,701),[352];(1069,704),[366];(1069,705),[368];(1069,706),[8];(1069,708),[741];(1069,709),[150];(1069,710),[157];(1069,711),[163];(1069,712),[206];(1069,713),[215];(1069,714),[224];(1069,717),[293];(1069,718),[322];(1069,721),[359];(1069,722),[390];(1069,723),[466];(1069,726),[544];(1069,730),[74];(1069,732),[682];(1069,733),[691];(1069,734),[704];(1069,735),[742];(1069,736),[747];(1069,737),[894];(1069,738),[151;152];(1069,739),[159];(1069,740),[164;165];(1069,741),[207];(1069,742),[216;217];(1069,743),[226];(1069,746),[295];(1069,747),[323;324];(1069,749),[360];(1069,750),[391;392];(1069,752),[467];(1069,755),[545];(1069,760),[684];(1069,761),[692];(1069,762),[75;76];(1069,763),[705;706];(1069,764),[743;744];(1069,765),[748];(1069,766),[895];(1069,767),[10];(1069,768),[154];(1069,769),[161];(1069,770),[167];(1069,772),[219];(1069,773),[228];(1069,776),[297];(1069,777),[326];(1069,780),[394];(1069,790),[686];(1069,792),[708];(1069,793),[746];(1069,795),[78];(1069,797),[12];(1069,798),[84;85];(1069,802),[528;529];(1069,806),[29];(1070,470),[70];(1070,673),[65];(1070,684),[67];(1070,695),[69];(1071,12),[268];(1071,32),[240];(1071,34),[387];(1071,67),[321];(1071,153),[1021];(1071,155),[1023];(1071,159),[1028];(1071,170),[1055;1056];(1071,378),[1393];(1071,454),[886];(1071,455),[405];(1071,456),[370];(1071,471),[378];(1071,476),[389];(1071,478),[409];(1071,479),[413];(1071,480),[417];(1071,483),[419];(1071,484),[423];(1071,486),[425];(1071,488),[429];(1071,489),[433];(1071,491),[435];(1071,493),[437];(1071,512),[512];(1071,585),[728];(1071,586),[726];(1071,587),[724];(1071,588),[730];(1071,622),[861];(1071,650),[214];(1071,652),[212];(1071,653),[223];(1071,654),[221];(1071,655),[229;230];(1071,656),[232];(1071,657),[239];(1071,658),[242];(1071,659),[245];(1071,660),[254;255];(1071,661),[249];(1071,663),[247];(1071,664),[253];(1071,665),[251];(1071,666),[258;259];(1071,667),[257];(1071,668),[261];(1071,669),[266;267];(1071,670),[265];(1071,671),[263];(1071,672),[270];(1071,674),[272];(1071,675),[276];(1071,676),[274];(1071,679),[291;292];(1071,681),[300];(1071,682),[306];(1071,683),[302];(1071,685),[304];(1071,686),[308];(1071,687),[309;310];(1071,688),[312];(1071,689),[318];(1071,690),[314];(1071,691),[316];(1071,692),[320];(1071,694),[330];(1071,696),[334];(1071,709),[150];(1071,713),[215];(1071,714),[224];(1071,717),[293];(1071,718),[322];(1071,722),[390];(1071,732),[682];(1071,738),[152];(1071,742),[217];(1071,743),[226];(1071,746),[295];(1071,747),[324];(1071,750),[392];(1071,760),[684];(1071,768),[154];(1071,772),[219];(1071,773),[228];(1071,776),[297];(1071,777),[326];(1071,780),[394];(1071,790),[686];(1071,797),[12];(1072,7),[196];(1072,8),[581];(1072,10),[561];(1072,12),[268];(1072,32),[240];(1072,34),[387];(1072,67),[321];(1072,76),[202];(1072,77),[632];(1072,122),[979];(1072,127),[987];(1072,128),[989;990];(1072,130),[993];(1072,131),[995];(1072,153),[1021];(1072,155),[1023];(1072,159),[1028];(1072,170),[1055];(1072,247),[1179];(1072,259),[1194];(1072,280),[1221];(1072,281),[1224];(1072,365),[1363];(1072,404),[1447];(1072,407),[1453];(1072,410),[1461];(1072,411),[1463];(1072,454),[886];(1072,455),[405];(1072,456),[370];(1072,459),[579;580];(1072,460),[602];(1072,461),[625];(1072,467),[181];(1072,471),[378];(1072,476),[389];(1072,478),[409];(1072,479),[413];(1072,480),[417];(1072,483),[419];(1072,484),[423];(1072,486),[425];(1072,488),[429];(1072,489),[433];(1072,491),[435];(1072,493),[437];(1072,512),[512];(1072,526),[564];(1072,527),[566];(1072,528),[570];(1072,530),[576];(1072,531),[572];(1072,532),[574];(1072,533),[578];(1072,534),[583];(1072,536),[585];(1072,537),[587];(1072,538),[594];(1072,541),[600];(1072,542),[604];(1072,544),[611];(1072,545),[609];(1072,547),[630;631];(1072,548),[613];(1072,549),[615];(1072,550),[617];(1072,551),[619];(1072,553),[623];(1072,554),[627];(1072,556),[644];(1072,585),[728];(1072,586),[726];(1072,587),[724];(1072,588),[730];(1072,612),[820];(1072,614),[822];(1072,615),[824];(1072,616),[826];(1072,617),[828];(1072,618),[830];(1072,622),[861];(1072,641),[185];(1072,642),[190;191];(1072,643),[187];(1072,644),[189];(1072,645),[193];(1072,646),[194;195];(1072,647),[198];(1072,648),[199;200];(1072,650),[214];(1072,652),[212];(1072,653),[223];(1072,654),[221];(1072,655),[229;230];(1072,656),[232];(1072,657),[239];(1072,658),[242];(1072,659),[245];(1072,660),[254;255];(1072,661),[249];(1072,663),[247];(1072,664),[253];(1072,665),[251];(1072,666),[258;259];(1072,667),[257];(1072,668),[261];(1072,669),[266;267];(1072,670),[265];(1072,671),[263];(1072,672),[270];(1072,674),[272];(1072,675),[276];(1072,676),[274];(1072,679),[291;292];(1072,681),[300];(1072,682),[306];(1072,683),[302];(1072,685),[304];(1072,686),[308];(1072,687),[309;310];(1072,688),[312];(1072,689),[318];(1072,690),[314];(1072,691),[316];(1072,692),[320];(1072,694),[330];(1072,696),[334];(1072,709),[150];(1072,713),[215];(1072,714),[224];(1072,717),[293];(1072,718),[322];(1072,722),[390];(1072,726),[544];(1072,727),[539];(1072,732),[682];(1072,738),[152];(1072,742),[217];(1072,743),[226];(1072,746),[295];(1072,747),[324];(1072,750),[392];(1072,755),[545;546];(1072,756),[540];(1072,760),[684];(1072,768),[154];(1072,772),[219];(1072,773),[228];(1072,776),[297];(1072,777),[326];(1072,780),[394];(1072,785),[548];(1072,790),[686];(1072,797),[12];(1073,12),[268];(1073,32),[240];(1073,34),[387];(1073,67),[321];(1073,153),[1021];(1073,155),[1023];(1073,159),[1028];(1073,170),[1055];(1073,454),[886];(1073,455),[401;405];(1073,456),[370];(1073,471),[378];(1073,476),[389];(1073,478),[409];(1073,479),[413];(1073,480),[417];(1073,483),[419];(1073,484),[423];(1073,486),[425];(1073,488),[429];(1073,489),[433];(1073,491),[435];(1073,493),[437];(1073,512),[512];(1073,585),[728];(1073,586),[726];(1073,587),[724];(1073,588),[730];(1073,622),[861];(1073,650),[214];(1073,652),[212];(1073,653),[223];(1073,654),[221];(1073,655),[229;230];(1073,656),[232];(1073,657),[239];(1073,658),[242];(1073,659),[245];(1073,660),[254;255];(1073,661),[249];(1073,663),[247];(1073,664),[253];(1073,665),[251];(1073,666),[258;259];(1073,667),[257];(1073,668),[261];(1073,669),[266;267];(1073,670),[265];(1073,671),[263];(1073,672),[270];(1073,674),[272];(1073,675),[276];(1073,676),[274];(1073,679),[291;292];(1073,681),[300];(1073,682),[306];(1073,683),[302];(1073,685),[304];(1073,686),[308];(1073,687),[309;310];(1073,688),[312];(1073,689),[318];(1073,690),[314];(1073,691),[316];(1073,692),[320];(1073,694),[330];(1073,696),[334];(1073,709),[150];(1073,713),[215];(1073,714),[224];(1073,717),[293];(1073,718),[322];(1073,722),[390];(1073,732),[682];(1073,738),[152];(1073,742),[217];(1073,743),[226];(1073,746),[295];(1073,747),[324];(1073,750),[392];(1073,760),[684];(1073,768),[154];(1073,772),[219];(1073,773),[228];(1073,776),[297];(1073,777),[326];(1073,780),[394];(1073,790),[686];(1073,797),[12];(1074,293),[1239];(1074,360),[1351];(1075,78),[503];(1075,240),[1154];(1075,463),[676];(1075,509),[505];(1076,1),[43];(1076,12),[268];(1076,32),[240];(1076,34),[387];(1076,58),[277];(1076,63),[28];(1076,67),[321];(1076,153),[1021];(1076,155),[1023];(1076,159),[1028];(1076,164),[1033];(1076,170),[1055];(1076,296),[1249];(1076,302),[1256];(1076,328),[1295];(1076,425),[1489];(1076,450),[899];(1076,454),[886];(1076,455),[405];(1076,456),[370];(1076,464),[32];(1076,465),[115];(1076,471),[378];(1076,476),[389];(1076,478),[409];(1076,479),[413];(1076,480),[417];(1076,481),[80];(1076,483),[419];(1076,484),[423];(1076,486),[425];(1076,488),[429];(1076,489),[433];(1076,491),[435];(1076,492),[73];(1076,493),[437];(1076,512),[512];(1076,522),[556];(1076,523),[558];(1076,525),[560];(1076,570),[679];(1076,571),[681];(1076,572),[688];(1076,573),[689;690];(1076,574),[697];(1076,575),[700];(1076,576),[702];(1076,577),[712];(1076,579),[31];(1076,581),[716];(1076,585),[728];(1076,586),[726];(1076,587),[724];(1076,588),[730];(1076,589),[733];(1076,590),[735];(1076,594),[757];(1076,595),[764;765];(1076,596),[759];(1076,597),[761];(1076,598),[763];(1076,599),[767];(1076,603),[777];(1076,604),[779];(1076,609),[794];(1076,610),[796];(1076,611),[798];(1076,612),[820];(1076,614),[822];(1076,615),[824];(1076,616),[826];(1076,617),[828];(1076,618),[830];(1076,622),[861];(1076,624),[113];(1076,627),[117];(1076,628),[137];(1076,629),[48];(1076,631),[146];(1076,633),[156];(1076,639),[179];(1076,640),[50];(1076,650),[214];(1076,651),[52];(1076,652),[212];(1076,653),[223];(1076,654),[221];(1076,655),[229;230];(1076,656),[232];(1076,657),[239];(1076,658),[242];(1076,659),[245];(1076,660),[254;255];(1076,661),[249];(1076,663),[247];(1076,664),[253];(1076,665),[251];(1076,666),[258;259];(1076,667),[257];(1076,668),[261];(1076,669),[266;267];(1076,670),[265];(1076,671),[263];(1076,672),[270];(1076,674),[272];(1076,675),[276];(1076,676),[274];(1076,679),[291;292];(1076,681),[300];(1076,682),[306];(1076,683),[302];(1076,685),[304];(1076,686),[308];(1076,687),[309;310];(1076,688),[312];(1076,689),[318];(1076,690),[314];(1076,691),[316];(1076,692),[320];(1076,694),[330];(1076,696),[334];(1076,708),[741];(1076,709),[150];(1076,710),[157];(1076,711),[163];(1076,713),[215];(1076,714),[224];(1076,715),[233];(1076,717),[293];(1076,718),[322];(1076,722),[390];(1076,730),[74];(1076,732),[682];(1076,734),[704];(1076,735),[742];(1076,738),[152];(1076,739),[159];(1076,740),[165];(1076,742),[217];(1076,743),[226];(1076,744),[234];(1076,746),[295];(1076,747),[324];(1076,750),[392];(1076,760),[684];(1076,762),[76];(1076,763),[706];(1076,764),[743;744];(1076,768),[154];(1076,769),[161];(1076,770),[167];(1076,772),[219];(1076,773),[228];(1076,776),[297];(1076,777),[326];(1076,780),[394];(1076,790),[686];(1076,792),[708];(1076,793),[746];(1076,795),[78];(1076,797),[12];(1076,806),[29];(1077,428),[1496];(1077,429),[1498];(1078,12),[268];(1078,32),[240];(1078,34),[387];(1078,67),[321];(1078,153),[1021];(1078,155),[1023];(1078,159),[1028];(1078,170),[1055];(1078,296),[1249];(1078,302),[1257];(1078,454),[886];(1078,455),[405];(1078,456),[370];(1078,471),[378];(1078,476),[389];(1078,478),[409];(1078,479),[413];(1078,480),[417];(1078,483),[419];(1078,484),[423];(1078,486),[425];(1078,488),[429];(1078,489),[433];(1078,491),[435];(1078,493),[437];(1078,512),[512];(1078,570),[679];(1078,571),[681];(1078,572),[688];(1078,573),[689];(1078,575),[700];(1078,576),[702];(1078,577),[712];(1078,581),[716];(1078,585),[728];(1078,586),[726];(1078,587),[724];(1078,588),[730];(1078,622),[861];(1078,650),[214];(1078,652),[212];(1078,653),[223];(1078,654),[221];(1078,655),[229;230];(1078,656),[232];(1078,657),[239];(1078,658),[242];(1078,659),[245];(1078,660),[254;255];(1078,661),[249];(1078,663),[247];(1078,664),[253];(1078,665),[251];(1078,666),[258;259];(1078,667),[257];(1078,668),[261];(1078,669),[266;267];(1078,670),[265];(1078,671),[263];(1078,672),[270];(1078,674),[272];(1078,675),[276];(1078,676),[274];(1078,679),[291;292];(1078,681),[300];(1078,682),[306];(1078,683),[302];(1078,685),[304];(1078,686),[308];(1078,687),[309;310];(1078,688),[312];(1078,689),[318];(1078,690),[314];(1078,691),[316];(1078,692),[320];(1078,694),[330];(1078,696),[334];(1078,709),[150];(1078,713),[215];(1078,714),[224];(1078,717),[293];(1078,718),[322];(1078,722),[390];(1078,732),[682];(1078,734),[704];(1078,738),[152];(1078,742),[217];(1078,743),[226];(1078,746),[295];(1078,747),[324];(1078,750),[392];(1078,760),[684];(1078,763),[706];(1078,768),[154];(1078,772),[219];(1078,773),[228];(1078,776),[297];(1078,777),[326];(1078,780),[394];(1078,790),[686];(1078,792),[708];(1078,797),[12];(1079,72),[803];(1079,355),[1342];(1079,433),[1506];(1079,513),[88];(1079,524),[89];(1079,634),[169];(1079,737),[894];(1079,766),[895];(1080,72),[804];(1080,355),[1342];(1080,433),[1506];(1080,513),[88];(1080,524),[89];(1080,634),[169];(1080,737),[894];(1080,766),[895];(1081,513),[88];(1081,524),[90];(1081,535),[91];(1081,612),[820];(1081,614),[822];(1081,615),[824];(1081,616),[826];(1081,617),[828];(1081,618),[830];(1083,12),[268];(1083,32),[240];(1083,34),[387];(1083,67),[321];(1083,153),[1021];(1083,155),[1023];(1083,159),[1028];(1083,170),[1055];(1083,244),[1175];(1083,454),[886];(1083,455),[405];(1083,456),[370];(1083,458),[534];(1083,471),[378];(1083,476),[389];(1083,478),[409];(1083,479),[413];(1083,480),[417];(1083,483),[419];(1083,484),[423];(1083,486),[425];(1083,488),[429];(1083,489),[433];(1083,491),[435];(1083,493),[437];(1083,512),[512];(1083,517),[533];(1083,585),[728];(1083,586),[726];(1083,587),[724];(1083,588),[730];(1083,622),[861];(1083,650),[214];(1083,652),[212];(1083,653),[223];(1083,654),[221];(1083,655),[229;230];(1083,656),[232];(1083,657),[239];(1083,658),[242];(1083,659),[245];(1083,660),[254;255];(1083,661),[249];(1083,663),[247];(1083,664),[253];(1083,665),[251];(1083,666),[258;259];(1083,667),[257];(1083,668),[261];(1083,669),[266;267];(1083,670),[265];(1083,671),[263];(1083,672),[270];(1083,674),[272];(1083,675),[276];(1083,676),[274];(1083,679),[291;292];(1083,681),[300];(1083,682),[306];(1083,683),[302];(1083,685),[304];(1083,686),[308];(1083,687),[309;310];(1083,688),[312];(1083,689),[318];(1083,690),[314];(1083,691),[316];(1083,692),[320];(1083,694),[330];(1083,696),[334];(1083,709),[150];(1083,713),[215];(1083,714),[224];(1083,717),[293];(1083,718),[322];(1083,722),[390];(1083,732),[682];(1083,738),[152];(1083,742),[217];(1083,743),[226];(1083,746),[295];(1083,747),[324];(1083,750),[392];(1083,760),[684];(1083,768),[154];(1083,772),[219];(1083,773),[228];(1083,776),[297];(1083,777),[326];(1083,780),[394];(1083,790),[686];(1083,797),[12];(1084,12),[268];(1084,32),[240];(1084,34),[387];(1084,67),[321];(1084,153),[1021];(1084,155),[1023];(1084,159),[1028];(1084,170),[1055];(1084,177),[1064];(1084,454),[886];(1084,455),[405];(1084,456),[370];(1084,471),[378];(1084,476),[389];(1084,478),[409];(1084,479),[413];(1084,480),[417];(1084,483),[419];(1084,484),[423];(1084,486),[425];(1084,488),[429];(1084,489),[433];(1084,491),[435];(1084,493),[437];(1084,512),[512];(1084,585),[728];(1084,586),[726];(1084,587),[724];(1084,588),[730];(1084,622),[861];(1084,650),[214];(1084,652),[212];(1084,653),[223];(1084,654),[221];(1084,655),[229;230];(1084,656),[232];(1084,657),[239];(1084,658),[242];(1084,659),[245];(1084,660),[254;255];(1084,661),[249];(1084,663),[247];(1084,664),[253];(1084,665),[251];(1084,666),[258;259];(1084,667),[257];(1084,668),[261];(1084,669),[266;267];(1084,670),[265];(1084,671),[263];(1084,672),[270];(1084,674),[272];(1084,675),[276];(1084,676),[274];(1084,679),[291;292];(1084,681),[300];(1084,682),[306];(1084,683),[302];(1084,685),[304];(1084,686),[308];(1084,687),[309;310];(1084,688),[311;312];(1084,689),[318];(1084,690),[314];(1084,691),[316];(1084,692),[320];(1084,694),[330];(1084,696),[334];(1084,709),[150];(1084,713),[215];(1084,714),[224];(1084,717),[293];(1084,718),[322];(1084,722),[390];(1084,732),[682];(1084,738),[152];(1084,742),[217];(1084,743),[226];(1084,746),[295];(1084,747),[324];(1084,750),[392];(1084,760),[684];(1084,768),[154];(1084,772),[219];(1084,773),[228];(1084,776),[297];(1084,777),[326];(1084,780),[394];(1084,790),[686];(1084,797),[12];(1085,12),[268];(1085,32),[240];(1085,34),[387];(1085,54),[122];(1085,67),[321];(1085,85),[138];(1085,110),[961];(1085,111),[962];(1085,153),[1021];(1085,155),[1023];(1085,159),[1028];(1085,170),[1055];(1085,296),[1249];(1085,328),[1295];(1085,425),[1489];(1085,454),[886];(1085,455),[405];(1085,456),[370];(1085,465),[114;115];(1085,466),[142];(1085,471),[378];(1085,476),[389];(1085,478),[409];(1085,479),[413];(1085,480),[417];(1085,481),[80];(1085,483),[419];(1085,484),[423];(1085,486),[425];(1085,488),[429];(1085,489),[433];(1085,491),[435];(1085,492),[73];(1085,493),[437];(1085,512),[512];(1085,522),[556];(1085,523),[558];(1085,525),[560];(1085,570),[679];(1085,571),[681];(1085,572),[688];(1085,573),[690];(1085,574),[697];(1085,575),[700];(1085,576),[702];(1085,577),[712];(1085,581),[716];(1085,585),[728];(1085,586),[726];(1085,587),[724];(1085,588),[730];(1085,589),[733];(1085,590),[735];(1085,594),[757];(1085,595),[764;765];(1085,596),[759];(1085,597),[761];(1085,598),[763];(1085,599),[767];(1085,603),[777];(1085,604),[779];(1085,609),[794];(1085,610),[796];(1085,611),[798];(1085,612),[820];(1085,614),[822];(1085,615),[824];(1085,616),[826];(1085,617),[828];(1085,618),[830];(1085,622),[861];(1085,624),[113];(1085,627),[117];(1085,628),[137];(1085,629),[48];(1085,630),[141];(1085,631),[146];(1085,633),[156];(1085,639),[179];(1085,640),[50];(1085,650),[214];(1085,651),[52];(1085,652),[212];(1085,653),[223];(1085,654),[221];(1085,655),[229;230];(1085,656),[232];(1085,657),[239];(1085,658),[242];(1085,659),[245];(1085,660),[254;255];(1085,661),[249];(1085,663),[247];(1085,664),[253];(1085,665),[251];(1085,666),[258;259];(1085,667),[257];(1085,668),[261];(1085,669),[266;267];(1085,670),[265];(1085,671),[263];(1085,672),[270];(1085,674),[272];(1085,675),[276];(1085,676),[274];(1085,679),[291;292];(1085,681),[300];(1085,682),[306];(1085,683),[302];(1085,685),[304];(1085,686),[308];(1085,687),[309;310];(1085,688),[312];(1085,689),[318];(1085,690),[314];(1085,691),[316];(1085,692),[320];(1085,694),[330];(1085,696),[334];(1085,708),[741];(1085,709),[150];(1085,710),[157];(1085,711),[163];(1085,713),[215];(1085,714),[224];(1085,717),[293];(1085,718),[322];(1085,722),[390];(1085,730),[74];(1085,732),[682];(1085,734),[704];(1085,735),[742];(1085,738),[152];(1085,739),[159];(1085,740),[165];(1085,742),[217];(1085,743),[226];(1085,746),[295];(1085,747),[324];(1085,750),[392];(1085,760),[684];(1085,762),[76];(1085,763),[706];(1085,764),[743;744];(1085,768),[154];(1085,769),[161];(1085,770),[167];(1085,772),[219];(1085,773),[228];(1085,776),[297];(1085,777),[326];(1085,780),[394];(1085,790),[686];(1085,792),[708];(1085,793),[746];(1085,795),[78];(1085,797),[12];(1086,1),[34];(1086,12),[268];(1086,24),[703];(1086,32),[240];(1086,34),[387];(1086,40),[667];(1086,43),[514;515];(1086,51),[373;374;375;376];(1086,54),[120;134];(1086,63),[28];(1086,66),[364];(1086,67),[321];(1086,68),[203];(1086,73),[874];(1086,79),[399];(1086,108),[959];(1086,110),[961];(1086,111),[962];(1086,117),[970];(1086,133),[997];(1086,134),[998];(1086,153),[1021];(1086,155),[1023];(1086,159),[1028];(1086,170),[1055];(1086,174),[1061];(1086,178),[1065];(1086,181),[1069];(1086,188),[1079];(1086,190),[1086];(1086,193),[1091];(1086,195),[1094];(1086,204),[1105];(1086,205),[1107];(1086,262),[1198;1199];(1086,274),[1212;1213];(1086,288),[1233;1234];(1086,296),[1249];(1086,297),[1250];(1086,299),[1252];(1086,302),[1256];(1086,303),[1258];(1086,308),[1266];(1086,309),[1268];(1086,315),[1277];(1086,321),[1286];(1086,328),[1295];(1086,329),[1296];(1086,355),[1341];(1086,381),[1397];(1086,382),[1400];(1086,385),[1407];(1086,387),[1411];(1086,419),[1479];(1086,420),[1482];(1086,421),[1484];(1086,424),[1488];(1086,425),[1489;1491];(1086,433),[1505];(1086,437),[1517];(1086,444),[1534];(1086,450),[899];(1086,454),[886];(1086,455),[405];(1086,456),[370];(1086,464),[32];(1086,465),[114;115];(1086,466),[142];(1086,468),[350];(1086,471),[378];(1086,476),[389];(1086,478),[409];(1086,479),[412;413];(1086,480),[417];(1086,481),[80];(1086,483),[419];(1086,484),[423];(1086,486),[425];(1086,488),[429];(1086,489),[433];(1086,491),[435];(1086,492),[73];(1086,493),[437];(1086,512),[512];(1086,513),[88];(1086,517),[532];(1086,522),[556];(1086,523),[558];(1086,524),[90];(1086,525),[560];(1086,535),[92];(1086,536),[585];(1086,537),[586];(1086,543),[606];(1086,546),[94];(1086,549),[615];(1086,550),[616];(1086,555),[629];(1086,557),[96];(1086,560),[649];(1086,564),[660];(1086,565),[668];(1086,568),[98];(1086,570),[678;679];(1086,571),[681];(1086,572),[688];(1086,573),[689;690];(1086,574),[697];(1086,575),[700];(1086,576),[702];(1086,577),[712];(1086,579),[31];(1086,581),[716];(1086,585),[728];(1086,586),[725;726];(1086,587),[724];(1086,588),[730];(1086,589),[733];(1086,590),[735];(1086,591),[105];(1086,594),[757];(1086,595),[764;765];(1086,596),[759];(1086,597),[761];(1086,598),[763];(1086,599),[766;767];(1086,600),[769];(1086,602),[108];(1086,603),[777];(1086,604),[779];(1086,609),[794];(1086,610),[796];(1086,611),[798];(1086,612),[820];(1086,614),[822];(1086,615),[824];(1086,616),[826];(1086,617),[828];(1086,618),[830];(1086,622),[861];(1086,624),[113];(1086,627),[117];(1086,628),[137];(1086,629),[48];(1086,630),[141];(1086,631),[146];(1086,633),[155;156];(1086,635),[171];(1086,636),[173];(1086,637),[175];(1086,639),[179];(1086,640),[50];(1086,649),[204];(1086,650),[214];(1086,651),[52];(1086,652),[212];(1086,653),[223];(1086,654),[221];(1086,655),[229;230];(1086,656),[232];(1086,657),[239];(1086,658),[242];(1086,659),[245];(1086,660),[254;255];(1086,661),[249];(1086,663),[247];(1086,664),[253];(1086,665),[251];(1086,666),[258;259];(1086,667),[257];(1086,668),[261];(1086,669),[266;267];(1086,670),[265];(1086,671),[263];(1086,672),[270];(1086,674),[272];(1086,675),[276];(1086,676),[274];(1086,679),[291;292];(1086,681),[300];(1086,682),[306];(1086,683),[302];(1086,685),[303;304];(1086,686),[308];(1086,687),[309;310];(1086,688),[312];(1086,689),[318];(1086,690),[314];(1086,691),[315;316];(1086,692),[320];(1086,694),[330];(1086,696),[334];(1086,698),[338];(1086,699),[345];(1086,700),[348];(1086,701),[352];(1086,704),[366];(1086,705),[368];(1086,707),[101];(1086,708),[741];(1086,709),[150];(1086,710),[157];(1086,711),[163];(1086,713),[215];(1086,714),[224];(1086,717),[293];(1086,718),[322];(1086,719),[57];(1086,720),[340];(1086,721),[359];(1086,722),[390];(1086,730),[74];(1086,732),[682];(1086,733),[691];(1086,734),[704];(1086,735),[742];(1086,736),[747];(1086,737),[894];(1086,738),[151;152];(1086,739),[159];(1086,740),[165];(1086,742),[217];(1086,743),[226];(1086,746),[295];(1086,747),[323;324];(1086,748),[341];(1086,749),[360];(1086,750),[391;392];(1086,751),[59];(1086,760),[684];(1086,761),[692];(1086,762),[76];(1086,763),[705;706];(1086,764),[743;744];(1086,765),[748];(1086,766),[895];(1086,768),[154];(1086,769),[161];(1086,770),[167];(1086,772),[219];(1086,773),[228];(1086,776),[297];(1086,777),[326];(1086,780),[394];(1086,784),[61];(1086,790),[686];(1086,792),[708];(1086,793),[746];(1086,795),[78];(1086,797),[12];(1086,802),[528;529];(1086,806),[29];(1087,453),[904];(1088,78),[503];(1088,240),[1153];(1088,463),[676];(1088,509),[505];(1089,86),[243];(1089,357),[1344];(1089,370),[1374];(1090,372),[1379];(1091,372),[1378];(1092,151),[1019];(1092,157),[1026];(1092,665),[250];(1092,671),[262];(1093,1),[44];(1093,5),[893];(1093,7),[196];(1093,9),[183];(1093,10),[562];(1093,12),[268];(1093,13),[283];(1093,24),[703];(1093,29),[739];(1093,30),[738];(1093,32),[240];(1093,34),[387];(1093,35),[9];(1093,37),[6];(1093,43),[514;515];(1093,44),[717];(1093,51),[373;374;375;376];(1093,54),[132;133];(1093,63),[28];(1093,66),[364];(1093,67),[321];(1093,69),[55];(1093,70),[53];(1093,73),[875;876];(1093,79),[398;399;400];(1093,89),[930];(1093,99),[948];(1093,100),[949];(1093,101),[950];(1093,102),[951];(1093,106),[957];(1093,110),[961];(1093,111),[962];(1093,120),[975;976];(1093,122),[980];(1093,127),[987];(1093,128),[989;990];(1093,138),[1003];(1093,153),[1021];(1093,155),[1023];(1093,159),[1028];(1093,167),[1050;1051];(1093,170),[1055];(1093,181),[1068;1069;1070];(1093,183),[1073];(1093,185),[1075];(1093,190),[1085];(1093,193),[1091];(1093,195),[1094];(1093,204),[1105];(1093,206),[1108];(1093,208),[1110];(1093,211),[1113];(1093,215),[1118];(1093,216),[1119];(1093,223),[1128];(1093,227),[1134];(1093,247),[1179];(1093,295),[1248];(1093,296),[1249];(1093,302),[1256];(1093,303),[1258];(1093,308),[1266;1267];(1093,309),[1268];(1093,316),[1278];(1093,319),[1284];(1093,321),[1286];(1093,328),[1295];(1093,344),[1317];(1093,355),[1341];(1093,363),[1359;1360];(1093,366),[1366];(1093,367),[1367];(1093,381),[1397;1398];(1093,382),[1399;1400;1401];(1093,384),[1405;1406];(1093,385),[1407];(1093,387),[1411];(1093,392),[1420];(1093,404),[1447];(1093,419),[1479];(1093,420),[1482;1483];(1093,421),[1484];(1093,422),[1486];(1093,423),[1487];(1093,424),[1488];(1093,425),[1489;1491];(1093,433),[1505];(1093,434),[1508];(1093,436),[1516];(1093,437),[1517;1518];(1093,438),[1519];(1093,444),[1534;1535];(1093,445),[1537];(1093,446),[1540];(1093,450),[899];(1093,454),[886];(1093,455),[405];(1093,456),[370];(1093,464),[32];(1093,465),[114;115];(1093,466),[142];(1093,467),[181];(1093,468),[350];(1093,471),[378];(1093,476),[389];(1093,477),[407];(1093,478),[409];(1093,479),[413];(1093,480),[416;417];(1093,481),[80];(1093,482),[415];(1093,483),[419];(1093,484),[422;423];(1093,485),[421];(1093,486),[425];(1093,487),[427];(1093,488),[429];(1093,489),[432;433];(1093,490),[431];(1093,491),[435];(1093,492),[73];(1093,493),[437];(1093,494),[444;445];(1093,495),[446;447];(1093,503),[82;83];(1093,512),[512];(1093,513),[87];(1093,517),[532];(1093,522),[556];(1093,523),[558];(1093,525),[560];(1093,543),[606];(1093,555),[629];(1093,564),[660];(1093,566),[673];(1093,567),[671];(1093,569),[675];(1093,570),[679];(1093,571),[681];(1093,572),[688];(1093,573),[689;690];(1093,574),[697];(1093,575),[700];(1093,576),[702];(1093,577),[712];(1093,579),[31];(1093,580),[102;103];(1093,581),[716];(1093,583),[721];(1093,585),[727;728];(1093,586),[726];(1093,587),[724];(1093,588),[730];(1093,589),[733];(1093,590),[735];(1093,592),[753];(1093,593),[755];(1093,594),[757];(1093,595),[764;765];(1093,596),[759];(1093,597),[761];(1093,598),[763];(1093,599),[767];(1093,600),[769];(1093,603),[777];(1093,604),[779];(1093,609),[794];(1093,610),[796];(1093,611),[798];(1093,612),[820];(1093,614),[822];(1093,615),[824];(1093,616),[826];(1093,617),[828];(1093,618),[830];(1093,622),[861];(1093,624),[113];(1093,627),[117];(1093,628),[137];(1093,629),[48];(1093,630),[141];(1093,631),[146];(1093,633),[156];(1093,635),[171];(1093,636),[173];(1093,637),[175];(1093,639),[179];(1093,640),[50];(1093,641),[185];(1093,642),[190;191];(1093,643),[187];(1093,644),[189];(1093,645),[193];(1093,646),[194;195];(1093,647),[198];(1093,648),[200];(1093,650),[214];(1093,651),[52];(1093,652),[212];(1093,653),[223];(1093,654),[221];(1093,655),[229;230];(1093,656),[232];(1093,657),[239];(1093,658),[242];(1093,659),[245];(1093,660),[254;255];(1093,661),[249];(1093,663),[247];(1093,664),[253];(1093,665),[251];(1093,666),[258;259];(1093,667),[257];(1093,668),[261];(1093,669),[266;267];(1093,670),[265];(1093,671),[263];(1093,672),[270];(1093,674),[272];(1093,675),[276];(1093,676),[274];(1093,679),[291;292];(1093,681),[300];(1093,682),[306];(1093,683),[302];(1093,685),[304];(1093,686),[308];(1093,687),[309;310];(1093,688),[312];(1093,689),[318];(1093,690),[314];(1093,691),[316];(1093,692),[320];(1093,693),[327;328];(1093,694),[330];(1093,696),[333;334];(1093,697),[332];(1093,698),[338];(1093,700),[348];(1093,701),[352];(1093,704),[366];(1093,705),[368];(1093,706),[7];(1093,708),[741];(1093,709),[150];(1093,710),[157];(1093,711),[163];(1093,712),[206];(1093,713),[215];(1093,714),[224];(1093,717),[293];(1093,718),[322];(1093,721),[359];(1093,722),[390];(1093,723),[466];(1093,726),[544];(1093,730),[74];(1093,732),[682];(1093,733),[691];(1093,734),[704];(1093,735),[742];(1093,736),[747];(1093,737),[894];(1093,738),[151;152];(1093,739),[159];(1093,740),[164;165];(1093,741),[207];(1093,742),[216;217];(1093,743),[226];(1093,746),[295];(1093,747),[323;324];(1093,749),[360];(1093,750),[391;392];(1093,752),[467];(1093,755),[545];(1093,760),[684];(1093,761),[692];(1093,762),[75;76];(1093,763),[705;706];(1093,764),[743;744];(1093,765),[748];(1093,766),[895];(1093,767),[10];(1093,768),[154];(1093,769),[161];(1093,770),[167];(1093,772),[219];(1093,773),[228];(1093,776),[297];(1093,777),[326];(1093,780),[394];(1093,790),[686];(1093,792),[708];(1093,793),[746];(1093,795),[78];(1093,797),[12];(1093,798),[84;85];(1093,802),[528;529];(1093,806),[29];(1094,1),[44];(1094,12),[268];(1094,24),[703];(1094,29),[739];(1094,30),[738];(1094,32),[240];(1094,34),[387];(1094,43),[514;515];(1094,51),[373;374;375;376];(1094,54),[132;133];(1094,55),[86];(1094,63),[28];(1094,66),[364];(1094,67),[321];(1094,69),[55];(1094,70),[53];(1094,73),[871];(1094,79),[395;396;399];(1094,88),[928];(1094,89),[931];(1094,110),[961];(1094,111),[962];(1094,113),[964];(1094,120),[977];(1094,153),[1021];(1094,155),[1023];(1094,159),[1028];(1094,165),[1048];(1094,170),[1055];(1094,181),[1069];(1094,193),[1091];(1094,195),[1094];(1094,204),[1105];(1094,296),[1249];(1094,302),[1256];(1094,303),[1258];(1094,305),[1263];(1094,308),[1266];(1094,309),[1268];(1094,316),[1278];(1094,318),[1281];(1094,319),[1284];(1094,321),[1286];(1094,324),[1290];(1094,326),[1292];(1094,328),[1295];(1094,330),[1297];(1094,332),[1300];(1094,333),[1301];(1094,336),[1306];(1094,337),[1308];(1094,349),[1325];(1094,355),[1341];(1094,363),[1361];(1094,377),[1390];(1094,381),[1397];(1094,382),[1400];(1094,385),[1407];(1094,387),[1411];(1094,419),[1479];(1094,420),[1482];(1094,421),[1484];(1094,422),[1485];(1094,423),[1487];(1094,424),[1488];(1094,425),[1489;1490;1491];(1094,426),[1493];(1094,427),[1495];(1094,431),[1501];(1094,432),[1503];(1094,433),[1505];(1094,437),[1517];(1094,444),[1534];(1094,445),[1536];(1094,450),[899];(1094,452),[902];(1094,454),[886];(1094,455),[405];(1094,456),[370];(1094,464),[32];(1094,465),[114;115];(1094,466),[142];(1094,471),[378];(1094,476),[389];(1094,478),[409];(1094,479),[413];(1094,480),[417];(1094,481),[80];(1094,483),[419];(1094,484),[423];(1094,486),[425];(1094,488),[429];(1094,489),[433];(1094,491),[435];(1094,492),[73];(1094,493),[437];(1094,512),[512];(1094,517),[532];(1094,522),[556];(1094,523),[558];(1094,525),[560];(1094,543),[606];(1094,555),[629];(1094,564),[660];(1094,570),[679];(1094,571),[681];(1094,572),[688];(1094,573),[689;690];(1094,574),[697];(1094,575),[700];(1094,576),[702];(1094,577),[712];(1094,579),[31];(1094,581),[716];(1094,585),[727;728];(1094,586),[726];(1094,587),[724];(1094,588),[730];(1094,589),[732;733];(1094,590),[735];(1094,591),[105];(1094,592),[753];(1094,593),[754;755];(1094,594),[757];(1094,595),[764;765];(1094,596),[758;759];(1094,597),[761];(1094,598),[763];(1094,599),[767];(1094,600),[768;769];(1094,601),[773];(1094,603),[777];(1094,604),[779];(1094,605),[782];(1094,606),[784];(1094,609),[794];(1094,610),[796];(1094,611),[798];(1094,612),[820];(1094,614),[822];(1094,615),[824];(1094,616),[826];(1094,617),[828];(1094,618),[830];(1094,619),[857];(1094,622),[861];(1094,624),[113];(1094,627),[117];(1094,628),[137];(1094,629),[48];(1094,630),[141];(1094,631),[146];(1094,633),[156];(1094,635),[171];(1094,636),[173];(1094,637),[175];(1094,639),[179];(1094,640),[49;50];(1094,650),[214];(1094,651),[52];(1094,652),[212];(1094,653),[223];(1094,654),[221];(1094,655),[229;230];(1094,656),[232];(1094,657),[239];(1094,658),[242];(1094,659),[245];(1094,660),[254;255];(1094,661),[249];(1094,663),[247];(1094,664),[253];(1094,665),[251];(1094,666),[258;259];(1094,667),[257];(1094,668),[261];(1094,669),[266;267];(1094,670),[265];(1094,671),[263];(1094,672),[270];(1094,674),[272];(1094,675),[276];(1094,676),[274];(1094,679),[291;292];(1094,681),[300];(1094,682),[306];(1094,683),[302];(1094,685),[304];(1094,686),[308];(1094,687),[309;310];(1094,688),[312];(1094,689),[318];(1094,690),[314];(1094,691),[316];(1094,692),[320];(1094,694),[330];(1094,696),[334];(1094,698),[338];(1094,700),[348];(1094,704),[366];(1094,705),[368];(1094,707),[101];(1094,708),[741];(1094,709),[150];(1094,710),[157];(1094,711),[163];(1094,713),[215];(1094,714),[224];(1094,716),[278];(1094,717),[293];(1094,718),[322];(1094,719),[57];(1094,721),[359];(1094,722),[390];(1094,730),[74];(1094,732),[682];(1094,733),[691];(1094,734),[704];(1094,735),[742];(1094,736),[747];(1094,737),[894];(1094,738),[151;152];(1094,739),[159];(1094,740),[164;165];(1094,742),[217];(1094,743),[226];(1094,745),[279];(1094,746),[295];(1094,747),[323;324];(1094,749),[360];(1094,750),[391;392];(1094,751),[58];(1094,760),[683;684];(1094,761),[692];(1094,762),[76];(1094,763),[705;706];(1094,764),[743;744];(1094,765),[748];(1094,766),[895];(1094,768),[154];(1094,769),[161];(1094,770),[167];(1094,772),[219];(1094,773),[228];(1094,776),[297];(1094,777),[326];(1094,780),[394];(1094,790),[686];(1094,792),[708];(1094,793),[746];(1094,795),[78];(1094,797),[12];(1094,802),[528;529];(1094,803),[774;775];(1094,804),[785;786];(1094,806),[29];(1095,12),[268];(1095,15),[480];(1095,24),[703];(1095,32),[240];(1095,34),[387];(1095,43),[513;514;515];(1095,50),[527];(1095,51),[373;374;375;376];(1095,54),[134];(1095,57),[288];(1095,66),[364];(1095,67),[321];(1095,68),[203];(1095,73),[874;877];(1095,79),[397;399];(1095,82),[799];(1095,83),[530];(1095,110),[961];(1095,111),[962];(1095,133),[997];(1095,148),[1015];(1095,149),[1016];(1095,153),[1021];(1095,155),[1023];(1095,156),[1024];(1095,159),[1028];(1095,162),[1031];(1095,168),[1052];(1095,169),[1053];(1095,170),[1055];(1095,171),[1057];(1095,181),[1069];(1095,188),[1079];(1095,190),[1086];(1095,193),[1091];(1095,195),[1093;1094];(1095,201),[1102];(1095,204),[1105];(1095,207),[1109];(1095,209),[1111];(1095,210),[1112];(1095,213),[1115];(1095,223),[1127];(1095,227),[1133];(1095,231),[1142];(1095,256),[1189];(1095,267),[1204];(1095,277),[1216];(1095,290),[1236];(1095,296),[1249];(1095,300),[1253];(1095,303),[1258];(1095,306),[1264];(1095,308),[1266];(1095,309),[1268];(1095,321),[1286];(1095,328),[1295];(1095,355),[1341];(1095,379),[1394];(1095,381),[1397];(1095,382),[1400];(1095,385),[1407];(1095,387),[1410;1411];(1095,419),[1479];(1095,420),[1482];(1095,421),[1484];(1095,424),[1488];(1095,425),[1489;1491];(1095,433),[1505];(1095,437),[1517];(1095,444),[1534];(1095,454),[886];(1095,455),[405];(1095,456),[370];(1095,465),[114;115];(1095,466),[142];(1095,468),[350];(1095,471),[378];(1095,474),[384];(1095,475),[385];(1095,476),[389];(1095,478),[409];(1095,479),[413];(1095,480),[417];(1095,481),[80];(1095,483),[418;419];(1095,484),[423];(1095,486),[424;425];(1095,488),[428;429];(1095,489),[433];(1095,491),[435];(1095,492),[73];(1095,493),[436;437];(1095,510),[508];(1095,512),[512];(1095,517),[532];(1095,522),[556];(1095,523),[558];(1095,525),[560];(1095,531),[571];(1095,541),[599];(1095,543),[606];(1095,553),[622];(1095,555),[629];(1095,558),[646];(1095,562),[653];(1095,564),[660];(1095,570),[679];(1095,571),[680;681];(1095,572),[688];(1095,573),[690];(1095,574),[697];(1095,575),[699;700];(1095,576),[702];(1095,577),[712];(1095,581),[716];(1095,585),[728];(1095,586),[726];(1095,587),[724];(1095,588),[730];(1095,589),[733];(1095,590),[735];(1095,591),[105];(1095,594),[757];(1095,595),[764;765];(1095,596),[759];(1095,597),[761];(1095,598),[763];(1095,599),[767];(1095,600),[769];(1095,603),[777];(1095,604),[779];(1095,609),[794];(1095,610),[796];(1095,611),[798];(1095,612),[819;820];(1095,614),[821;822];(1095,615),[823;824];(1095,616),[825;826];(1095,617),[827;828];(1095,618),[829;830];(1095,622),[861];(1095,624),[113];(1095,627),[117];(1095,628),[137];(1095,629),[48];(1095,630),[141];(1095,631),[146];(1095,633),[156];(1095,635),[171];(1095,636),[173];(1095,637),[175];(1095,639),[179];(1095,640),[50];(1095,649),[205];(1095,650),[214];(1095,651),[52];(1095,652),[212];(1095,653),[223];(1095,654),[221];(1095,655),[229;230];(1095,656),[232];(1095,657),[239];(1095,658),[242];(1095,659),[244;245];(1095,660),[254;255];(1095,661),[249];(1095,663),[246;247];(1095,664),[253];(1095,665),[251];(1095,666),[258;259];(1095,667),[257];(1095,668),[260;261];(1095,669),[266;267];(1095,670),[265];(1095,671),[263];(1095,672),[270];(1095,674),[272];(1095,675),[276];(1095,676),[273;274];(1095,677),[284];(1095,678),[287];(1095,679),[291;292];(1095,681),[300];(1095,682),[306];(1095,683),[302];(1095,685),[304];(1095,686),[308];(1095,687),[309;310];(1095,688),[312];(1095,689),[318];(1095,690),[314];(1095,691),[316];(1095,692),[320];(1095,694),[330];(1095,696),[334];(1095,698),[338];(1095,699),[345];(1095,700),[348];(1095,701),[352];(1095,704),[366];(1095,705),[368];(1095,707),[100];(1095,708),[740;741];(1095,709),[150];(1095,710),[157];(1095,711),[163];(1095,712),[206];(1095,713),[215];(1095,714),[224];(1095,717),[293];(1095,718),[322];(1095,720),[340];(1095,721),[359];(1095,722),[390];(1095,730),[74];(1095,732),[682];(1095,733),[691];(1095,734),[704];(1095,735),[742];(1095,736),[747];(1095,737),[894];(1095,738),[151;152];(1095,739),[159];(1095,740),[165];(1095,741),[208];(1095,742),[217];(1095,743),[226];(1095,746),[294;295];(1095,747),[323;324];(1095,748),[341];(1095,749),[360];(1095,750),[391;392];(1095,760),[684];(1095,761),[692];(1095,762),[76];(1095,763),[705;706];(1095,764),[743;744];(1095,765),[748];(1095,766),[895];(1095,768),[154];(1095,769),[161];(1095,770),[167];(1095,771),[210];(1095,772),[219];(1095,773),[228];(1095,776),[297];(1095,777),[326];(1095,780),[394];(1095,790),[686];(1095,792),[708];(1095,793),[746];(1095,795),[78];(1095,797),[12];(1095,800),[482;483];(1095,802),[528;529];(1097,3),[847];(1097,12),[268];(1097,32),[240];(1097,34),[387];(1097,67),[321];(1097,153),[1021];(1097,155),[1023];(1097,159),[1028];(1097,170),[1055];(1097,454),[885;886];(1097,455),[405];(1097,456),[370];(1097,471),[378];(1097,476),[389];(1097,478),[409];(1097,479),[413];(1097,480),[417];(1097,483),[419];(1097,484),[423];(1097,486),[425];(1097,488),[429];(1097,489),[433];(1097,491),[435];(1097,493),[437];(1097,512),[512];(1097,585),[728];(1097,586),[726];(1097,587),[724];(1097,588),[730];(1097,622),[861];(1097,650),[214];(1097,652),[212];(1097,653),[223];(1097,654),[221];(1097,655),[229;230];(1097,656),[232];(1097,657),[239];(1097,658),[242];(1097,659),[245];(1097,660),[254;255];(1097,661),[249];(1097,663),[247];(1097,664),[253];(1097,665),[251];(1097,666),[258;259];(1097,667),[257];(1097,668),[261];(1097,669),[266;267];(1097,670),[265];(1097,671),[263];(1097,672),[270];(1097,674),[272];(1097,675),[276];(1097,676),[274];(1097,679),[291;292];(1097,681),[300];(1097,682),[306];(1097,683),[302];(1097,685),[304];(1097,686),[308];(1097,687),[309;310];(1097,688),[312];(1097,689),[318];(1097,690),[314];(1097,691),[316];(1097,692),[320];(1097,694),[330];(1097,696),[334];(1097,709),[150];(1097,713),[215];(1097,714),[224];(1097,717),[293];(1097,718),[322];(1097,722),[390];(1097,732),[682];(1097,738),[152];(1097,742),[217];(1097,743),[226];(1097,746),[295];(1097,747),[324];(1097,750),[392];(1097,760),[684];(1097,768),[154];(1097,772),[219];(1097,773),[228];(1097,776),[297];(1097,777),[326];(1097,780),[394];(1097,790),[686];(1097,797),[12];(1098,3),[840];(1098,12),[268];(1098,32),[240];(1098,34),[387];(1098,67),[321];(1098,153),[1021];(1098,155),[1023];(1098,159),[1028];(1098,170),[1055];(1098,454),[885;886];(1098,455),[405];(1098,456),[370];(1098,471),[378];(1098,476),[389];(1098,478),[409];(1098,479),[413];(1098,480),[417];(1098,483),[419];(1098,484),[423];(1098,486),[425];(1098,488),[429];(1098,489),[433];(1098,491),[435];(1098,493),[437];(1098,512),[512];(1098,585),[728];(1098,586),[726];(1098,587),[724];(1098,588),[730];(1098,622),[861];(1098,650),[214];(1098,652),[212];(1098,653),[223];(1098,654),[221];(1098,655),[229;230];(1098,656),[232];(1098,657),[239];(1098,658),[242];(1098,659),[245];(1098,660),[254;255];(1098,661),[249];(1098,663),[247];(1098,664),[253];(1098,665),[251];(1098,666),[258;259];(1098,667),[257];(1098,668),[261];(1098,669),[266;267];(1098,670),[265];(1098,671),[263];(1098,672),[270];(1098,674),[272];(1098,675),[276];(1098,676),[274];(1098,679),[291;292];(1098,681),[300];(1098,682),[306];(1098,683),[302];(1098,685),[304];(1098,686),[308];(1098,687),[309;310];(1098,688),[312];(1098,689),[318];(1098,690),[314];(1098,691),[316];(1098,692),[320];(1098,694),[330];(1098,696),[334];(1098,709),[150];(1098,713),[215];(1098,714),[224];(1098,717),[293];(1098,718),[322];(1098,722),[390];(1098,732),[682];(1098,738),[152];(1098,742),[217];(1098,743),[226];(1098,746),[295];(1098,747),[324];(1098,750),[392];(1098,760),[684];(1098,768),[154];(1098,772),[219];(1098,773),[228];(1098,776),[297];(1098,777),[326];(1098,780),[394];(1098,790),[686];(1098,797),[12];(1099,3),[849];(1099,12),[268];(1099,32),[240];(1099,34),[387];(1099,67),[321];(1099,153),[1021];(1099,155),[1023];(1099,159),[1028];(1099,170),[1055];(1099,454),[885;886];(1099,455),[405];(1099,456),[370];(1099,471),[378];(1099,476),[389];(1099,478),[409];(1099,479),[413];(1099,480),[417];(1099,483),[419];(1099,484),[423];(1099,486),[425];(1099,488),[429];(1099,489),[433];(1099,491),[435];(1099,493),[437];(1099,512),[512];(1099,585),[728];(1099,586),[726];(1099,587),[724];(1099,588),[730];(1099,622),[861];(1099,650),[214];(1099,652),[212];(1099,653),[223];(1099,654),[221];(1099,655),[229;230];(1099,656),[232];(1099,657),[239];(1099,658),[242];(1099,659),[245];(1099,660),[254;255];(1099,661),[249];(1099,663),[247];(1099,664),[253];(1099,665),[251];(1099,666),[258;259];(1099,667),[257];(1099,668),[261];(1099,669),[266;267];(1099,670),[265];(1099,671),[263];(1099,672),[270];(1099,674),[272];(1099,675),[276];(1099,676),[274];(1099,679),[291;292];(1099,681),[300];(1099,682),[306];(1099,683),[302];(1099,685),[304];(1099,686),[308];(1099,687),[309;310];(1099,688),[312];(1099,689),[318];(1099,690),[314];(1099,691),[316];(1099,692),[320];(1099,694),[330];(1099,696),[334];(1099,709),[150];(1099,713),[215];(1099,714),[224];(1099,717),[293];(1099,718),[322];(1099,722),[390];(1099,732),[682];(1099,738),[152];(1099,742),[217];(1099,743),[226];(1099,746),[295];(1099,747),[324];(1099,750),[392];(1099,760),[684];(1099,768),[154];(1099,772),[219];(1099,773),[228];(1099,776),[297];(1099,777),[326];(1099,780),[394];(1099,790),[686];(1099,797),[12];(1100,3),[844];(1100,12),[268];(1100,32),[240];(1100,34),[387];(1100,67),[321];(1100,103),[952];(1100,153),[1021];(1100,155),[1023];(1100,159),[1028];(1100,170),[1055];(1100,454),[885;886];(1100,455),[405];(1100,456),[370];(1100,471),[378];(1100,476),[389];(1100,478),[409];(1100,479),[413];(1100,480),[417];(1100,483),[419];(1100,484),[423];(1100,486),[425];(1100,488),[429];(1100,489),[433];(1100,491),[435];(1100,493),[437];(1100,512),[512];(1100,513),[88];(1100,524),[90];(1100,535),[92];(1100,546),[93];(1100,585),[728];(1100,586),[726];(1100,587),[724];(1100,588),[730];(1100,612),[820];(1100,614),[822];(1100,615),[824];(1100,616),[826];(1100,617),[828];(1100,618),[830];(1100,622),[861];(1100,650),[214];(1100,652),[212];(1100,653),[223];(1100,654),[221];(1100,655),[229;230];(1100,656),[232];(1100,657),[239];(1100,658),[242];(1100,659),[245];(1100,660),[254;255];(1100,661),[249];(1100,662),[63];(1100,663),[247];(1100,664),[253];(1100,665),[251];(1100,666),[258;259];(1100,667),[257];(1100,668),[261];(1100,669),[266;267];(1100,670),[265];(1100,671),[263];(1100,672),[270];(1100,674),[272];(1100,675),[276];(1100,676),[274];(1100,679),[291;292];(1100,681),[300];(1100,682),[306];(1100,683),[302];(1100,685),[304];(1100,686),[308];(1100,687),[309;310];(1100,688),[312];(1100,689),[318];(1100,690),[314];(1100,691),[316];(1100,692),[320];(1100,694),[330];(1100,696),[334];(1100,709),[150];(1100,713),[215];(1100,714),[224];(1100,717),[293];(1100,718),[322];(1100,722),[390];(1100,732),[682];(1100,738),[152];(1100,742),[217];(1100,743),[226];(1100,746),[295];(1100,747),[324];(1100,750),[392];(1100,760),[684];(1100,768),[154];(1100,772),[219];(1100,773),[228];(1100,776),[297];(1100,777),[326];(1100,780),[394];(1100,790),[686];(1100,797),[12];(1102,3),[832;834;835;837];(1102,12),[268];(1102,32),[240];(1102,34),[387];(1102,67),[321];(1102,153),[1021];(1102,155),[1023];(1102,159),[1028];(1102,170),[1055];(1102,345),[1321];(1102,346),[1322];(1102,348),[1324];(1102,454),[885;886];(1102,455),[405];(1102,456),[370];(1102,471),[378];(1102,476),[389];(1102,478),[409];(1102,479),[413];(1102,480),[417];(1102,483),[419];(1102,484),[423];(1102,486),[425];(1102,488),[429];(1102,489),[433];(1102,491),[435];(1102,493),[437];(1102,512),[512];(1102,585),[728];(1102,586),[726];(1102,587),[724];(1102,588),[730];(1102,622),[861];(1102,650),[214];(1102,652),[212];(1102,653),[223];(1102,654),[221];(1102,655),[229;230];(1102,656),[232];(1102,657),[239];(1102,658),[242];(1102,659),[245];(1102,660),[254;255];(1102,661),[249];(1102,663),[247];(1102,664),[253];(1102,665),[251];(1102,666),[258;259];(1102,667),[257];(1102,668),[261];(1102,669),[266;267];(1102,670),[265];(1102,671),[263];(1102,672),[270];(1102,674),[272];(1102,675),[276];(1102,676),[274];(1102,679),[291;292];(1102,681),[300];(1102,682),[306];(1102,683),[302];(1102,685),[304];(1102,686),[308];(1102,687),[309;310];(1102,688),[312];(1102,689),[318];(1102,690),[314];(1102,691),[316];(1102,692),[320];(1102,694),[330];(1102,696),[334];(1102,709),[150];(1102,713),[215];(1102,714),[224];(1102,717),[293];(1102,718),[322];(1102,722),[390];(1102,732),[682];(1102,738),[152];(1102,742),[217];(1102,743),[226];(1102,746),[295];(1102,747),[324];(1102,750),[392];(1102,760),[684];(1102,768),[154];(1102,772),[219];(1102,773),[228];(1102,776),[297];(1102,777),[326];(1102,780),[394];(1102,790),[686];(1102,797),[12];(1103,3),[842];(1103,12),[268];(1103,20),[0];(1103,24),[703];(1103,32),[240];(1103,34),[387];(1103,43),[514;515];(1103,51),[373;374;375;376];(1103,66),[364];(1103,67),[321];(1103,73),[879];(1103,74),[4];(1103,79),[399];(1103,113),[966];(1103,153),[1021];(1103,155),[1023];(1103,159),[1028];(1103,170),[1055];(1103,181),[1069];(1103,193),[1091];(1103,195),[1094];(1103,198),[1099];(1103,204),[1105];(1103,303),[1258];(1103,308),[1266];(1103,309),[1268];(1103,321),[1286];(1103,328),[1295];(1103,355),[1341];(1103,381),[1397];(1103,382),[1400];(1103,385),[1407];(1103,387),[1411];(1103,388),[1413];(1103,389),[1415];(1103,419),[1479];(1103,420),[1482];(1103,421),[1484];(1103,424),[1488];(1103,425),[1491];(1103,431),[1502];(1103,432),[1504];(1103,433),[1505];(1103,437),[1517];(1103,444),[1534];(1103,454),[885;886];(1103,455),[405];(1103,456),[370];(1103,469),[1];(1103,471),[378];(1103,476),[389];(1103,478),[409];(1103,479),[413];(1103,480),[417];(1103,483),[419];(1103,484),[423];(1103,486),[425];(1103,488),[429];(1103,489),[433];(1103,491),[435];(1103,493),[437];(1103,512),[512];(1103,517),[532];(1103,543),[606];(1103,555),[629];(1103,564),[660];(1103,577),[712];(1103,585),[728];(1103,586),[726];(1103,587),[724];(1103,588),[730];(1103,594),[757];(1103,595),[764];(1103,596),[759];(1103,600),[769];(1103,622),[861];(1103,635),[171];(1103,636),[173];(1103,637),[175];(1103,650),[214];(1103,652),[212];(1103,653),[223];(1103,654),[221];(1103,655),[229;230];(1103,656),[232];(1103,657),[239];(1103,658),[242];(1103,659),[245];(1103,660),[254;255];(1103,661),[249];(1103,663),[247];(1103,664),[253];(1103,665),[251];(1103,666),[258;259];(1103,667),[257];(1103,668),[261];(1103,669),[266;267];(1103,670),[265];(1103,671),[263];(1103,672),[270];(1103,674),[272];(1103,675),[276];(1103,676),[274];(1103,679),[291;292];(1103,681),[300];(1103,682),[306];(1103,683),[302];(1103,685),[304];(1103,686),[308];(1103,687),[309;310];(1103,688),[312];(1103,689),[318];(1103,690),[314];(1103,691),[316];(1103,692),[320];(1103,694),[330];(1103,696),[334];(1103,698),[338];(1103,700),[348];(1103,704),[366];(1103,705),[368];(1103,708),[741];(1103,709),[150];(1103,713),[215];(1103,714),[224];(1103,717),[293];(1103,718),[322];(1103,721),[359];(1103,722),[390];(1103,732),[682];(1103,733),[691];(1103,734),[704];(1103,735),[742];(1103,736),[747];(1103,737),[894];(1103,738),[151;152];(1103,742),[217];(1103,743),[226];(1103,746),[295];(1103,747),[323;324];(1103,749),[360];(1103,750),[391;392];(1103,760),[684];(1103,761),[692];(1103,763),[705];(1103,764),[743];(1103,765),[748];(1103,766),[895];(1103,768),[154];(1103,772),[219];(1103,773),[228];(1103,776),[297];(1103,777),[326];(1103,780),[394];(1103,790),[686];(1103,797),[12];(1103,802),[528;529];(1104,3),[851];(1104,12),[268];(1104,24),[703];(1104,32),[240];(1104,34),[387];(1104,43),[514;515];(1104,51),[373;374;375;376];(1104,66),[364];(1104,67),[321];(1104,73),[873];(1104,79),[399];(1104,153),[1021];(1104,155),[1023];(1104,159),[1028];(1104,170),[1055];(1104,181),[1069];(1104,193),[1091];(1104,195),[1094];(1104,204),[1105];(1104,303),[1258];(1104,308),[1266];(1104,309),[1268];(1104,321),[1286];(1104,328),[1295];(1104,353),[1336];(1104,355),[1341];(1104,381),[1397];(1104,382),[1400];(1104,385),[1407];(1104,387),[1411];(1104,419),[1479];(1104,420),[1482];(1104,421),[1484];(1104,424),[1488];(1104,425),[1491];(1104,433),[1505];(1104,437),[1517];(1104,444),[1534];(1104,454),[885;886];(1104,455),[405];(1104,456),[370];(1104,471),[378];(1104,476),[389];(1104,478),[409];(1104,479),[413];(1104,480),[417];(1104,483),[419];(1104,484),[423];(1104,486),[425];(1104,488),[429];(1104,489),[433];(1104,491),[435];(1104,493),[437];(1104,512),[512];(1104,517),[532];(1104,543),[606];(1104,555),[629];(1104,564),[660];(1104,577),[712];(1104,585),[728];(1104,586),[726];(1104,587),[724];(1104,588),[730];(1104,594),[757];(1104,595),[764];(1104,596),[759];(1104,600),[769];(1104,622),[861];(1104,635),[171];(1104,636),[173];(1104,637),[175];(1104,650),[214];(1104,652),[212];(1104,653),[223];(1104,654),[221];(1104,655),[229;230];(1104,656),[232];(1104,657),[239];(1104,658),[242];(1104,659),[245];(1104,660),[254;255];(1104,661),[249];(1104,663),[247];(1104,664),[253];(1104,665),[251];(1104,666),[258;259];(1104,667),[257];(1104,668),[261];(1104,669),[266;267];(1104,670),[265];(1104,671),[263];(1104,672),[270];(1104,674),[272];(1104,675),[276];(1104,676),[274];(1104,679),[291;292];(1104,681),[300];(1104,682),[306];(1104,683),[302];(1104,685),[304];(1104,686),[308];(1104,687),[309;310];(1104,688),[312];(1104,689),[318];(1104,690),[314];(1104,691),[316];(1104,692),[320];(1104,694),[330];(1104,696),[334];(1104,698),[338];(1104,700),[348];(1104,704),[366];(1104,705),[368];(1104,708),[741];(1104,709),[150];(1104,713),[215];(1104,714),[224];(1104,717),[293];(1104,718),[322];(1104,721),[359];(1104,722),[390];(1104,732),[682];(1104,733),[691];(1104,734),[704];(1104,735),[742];(1104,736),[747];(1104,737),[894];(1104,738),[151;152];(1104,742),[217];(1104,743),[226];(1104,746),[295];(1104,747),[323;324];(1104,749),[360];(1104,750),[391;392];(1104,760),[684];(1104,761),[692];(1104,763),[705];(1104,764),[743];(1104,765),[748];(1104,766),[895];(1104,768),[154];(1104,772),[219];(1104,773),[228];(1104,776),[297];(1104,777),[326];(1104,780),[394];(1104,790),[686];(1104,797),[12];(1104,802),[528;529];(1105,3),[839];(1105,12),[268];(1105,32),[240];(1105,34),[387];(1105,67),[321];(1105,153),[1021];(1105,155),[1023];(1105,159),[1028];(1105,170),[1055];(1105,454),[885;886];(1105,455),[405];(1105,456),[370];(1105,471),[378];(1105,476),[389];(1105,478),[409];(1105,479),[413];(1105,480),[417];(1105,483),[419];(1105,484),[423];(1105,486),[425];(1105,488),[429];(1105,489),[433];(1105,491),[435];(1105,493),[437];(1105,512),[512];(1105,585),[728];(1105,586),[726];(1105,587),[724];(1105,588),[730];(1105,622),[861];(1105,650),[214];(1105,652),[212];(1105,653),[223];(1105,654),[221];(1105,655),[229;230];(1105,656),[232];(1105,657),[239];(1105,658),[242];(1105,659),[245];(1105,660),[254;255];(1105,661),[249];(1105,663),[247];(1105,664),[253];(1105,665),[251];(1105,666),[258;259];(1105,667),[257];(1105,668),[261];(1105,669),[266;267];(1105,670),[265];(1105,671),[263];(1105,672),[270];(1105,674),[272];(1105,675),[276];(1105,676),[274];(1105,679),[291;292];(1105,681),[300];(1105,682),[306];(1105,683),[302];(1105,685),[304];(1105,686),[308];(1105,687),[309;310];(1105,688),[312];(1105,689),[318];(1105,690),[314];(1105,691),[316];(1105,692),[320];(1105,694),[330];(1105,696),[334];(1105,709),[150];(1105,713),[215];(1105,714),[224];(1105,717),[293];(1105,718),[322];(1105,722),[390];(1105,732),[682];(1105,738),[152];(1105,742),[217];(1105,743),[226];(1105,746),[295];(1105,747),[324];(1105,750),[392];(1105,760),[684];(1105,768),[154];(1105,772),[219];(1105,773),[228];(1105,776),[297];(1105,777),[326];(1105,780),[394];(1105,790),[686];(1105,797),[12];(1106,3),[848];(1106,12),[268];(1106,32),[240];(1106,34),[387];(1106,67),[321];(1106,153),[1021];(1106,155),[1023];(1106,159),[1028];(1106,170),[1055];(1106,454),[885;886];(1106,455),[405];(1106,456),[370];(1106,471),[378];(1106,476),[389];(1106,478),[409];(1106,479),[413];(1106,480),[417];(1106,483),[419];(1106,484),[423];(1106,486),[425];(1106,488),[429];(1106,489),[433];(1106,491),[435];(1106,493),[437];(1106,512),[512];(1106,585),[728];(1106,586),[726];(1106,587),[724];(1106,588),[730];(1106,622),[861];(1106,650),[214];(1106,652),[212];(1106,653),[223];(1106,654),[221];(1106,655),[229;230];(1106,656),[232];(1106,657),[239];(1106,658),[242];(1106,659),[245];(1106,660),[254;255];(1106,661),[249];(1106,663),[247];(1106,664),[253];(1106,665),[251];(1106,666),[258;259];(1106,667),[257];(1106,668),[261];(1106,669),[266;267];(1106,670),[265];(1106,671),[263];(1106,672),[270];(1106,674),[272];(1106,675),[276];(1106,676),[274];(1106,679),[291;292];(1106,681),[300];(1106,682),[306];(1106,683),[302];(1106,685),[304];(1106,686),[308];(1106,687),[309;310];(1106,688),[312];(1106,689),[318];(1106,690),[314];(1106,691),[316];(1106,692),[320];(1106,694),[330];(1106,696),[334];(1106,709),[150];(1106,713),[215];(1106,714),[224];(1106,717),[293];(1106,718),[322];(1106,722),[390];(1106,732),[682];(1106,738),[152];(1106,742),[217];(1106,743),[226];(1106,746),[295];(1106,747),[324];(1106,750),[392];(1106,760),[684];(1106,768),[154];(1106,772),[219];(1106,773),[228];(1106,776),[297];(1106,777),[326];(1106,780),[394];(1106,790),[686];(1106,797),[12];(1107,3),[833;836];(1107,12),[268];(1107,32),[240];(1107,34),[387];(1107,67),[321];(1107,153),[1021];(1107,155),[1023];(1107,159),[1028];(1107,170),[1055];(1107,347),[1323];(1107,454),[885;886];(1107,455),[405];(1107,456),[370];(1107,471),[378];(1107,476),[389];(1107,478),[409];(1107,479),[413];(1107,480),[417];(1107,483),[419];(1107,484),[423];(1107,486),[425];(1107,488),[429];(1107,489),[433];(1107,491),[435];(1107,493),[437];(1107,512),[512];(1107,585),[728];(1107,586),[726];(1107,587),[724];(1107,588),[730];(1107,622),[861];(1107,650),[214];(1107,652),[212];(1107,653),[223];(1107,654),[221];(1107,655),[229;230];(1107,656),[232];(1107,657),[239];(1107,658),[242];(1107,659),[245];(1107,660),[254;255];(1107,661),[249];(1107,663),[247];(1107,664),[253];(1107,665),[251];(1107,666),[258;259];(1107,667),[257];(1107,668),[261];(1107,669),[266;267];(1107,670),[265];(1107,671),[263];(1107,672),[270];(1107,674),[272];(1107,675),[276];(1107,676),[274];(1107,679),[291;292];(1107,681),[300];(1107,682),[306];(1107,683),[302];(1107,685),[304];(1107,686),[308];(1107,687),[309;310];(1107,688),[312];(1107,689),[318];(1107,690),[314];(1107,691),[316];(1107,692),[320];(1107,694),[330];(1107,696),[334];(1107,709),[150];(1107,713),[215];(1107,714),[224];(1107,717),[293];(1107,718),[322];(1107,722),[390];(1107,732),[682];(1107,738),[152];(1107,742),[217];(1107,743),[226];(1107,746),[295];(1107,747),[324];(1107,750),[392];(1107,760),[684];(1107,768),[154];(1107,772),[219];(1107,773),[228];(1107,776),[297];(1107,777),[326];(1107,780),[394];(1107,790),[686];(1107,797),[12];(1108,3),[850];(1108,12),[268];(1108,32),[240];(1108,34),[387];(1108,67),[321];(1108,153),[1021];(1108,155),[1023];(1108,159),[1028];(1108,170),[1055];(1108,454),[885;886];(1108,455),[405];(1108,456),[370];(1108,471),[378];(1108,476),[389];(1108,478),[409];(1108,479),[413];(1108,480),[417];(1108,483),[419];(1108,484),[423];(1108,486),[425];(1108,488),[429];(1108,489),[433];(1108,491),[435];(1108,493),[437];(1108,512),[512];(1108,585),[728];(1108,586),[726];(1108,587),[724];(1108,588),[730];(1108,622),[861];(1108,650),[214];(1108,652),[212];(1108,653),[223];(1108,654),[221];(1108,655),[229;230];(1108,656),[232];(1108,657),[239];(1108,658),[242];(1108,659),[245];(1108,660),[254;255];(1108,661),[249];(1108,663),[247];(1108,664),[253];(1108,665),[251];(1108,666),[258;259];(1108,667),[257];(1108,668),[261];(1108,669),[266;267];(1108,670),[265];(1108,671),[263];(1108,672),[270];(1108,674),[272];(1108,675),[276];(1108,676),[274];(1108,679),[291;292];(1108,681),[300];(1108,682),[306];(1108,683),[302];(1108,685),[304];(1108,686),[308];(1108,687),[309;310];(1108,688),[312];(1108,689),[318];(1108,690),[314];(1108,691),[316];(1108,692),[320];(1108,694),[330];(1108,696),[334];(1108,709),[150];(1108,713),[215];(1108,714),[224];(1108,717),[293];(1108,718),[322];(1108,722),[390];(1108,732),[682];(1108,738),[152];(1108,742),[217];(1108,743),[226];(1108,746),[295];(1108,747),[324];(1108,750),[392];(1108,760),[684];(1108,768),[154];(1108,772),[219];(1108,773),[228];(1108,776),[297];(1108,777),[326];(1108,780),[394];(1108,790),[686];(1108,797),[12];(1109,3),[845];(1109,12),[268];(1109,32),[240];(1109,34),[387];(1109,67),[321];(1109,153),[1021];(1109,155),[1023];(1109,159),[1028];(1109,170),[1055];(1109,454),[885;886];(1109,455),[405];(1109,456),[370];(1109,471),[378];(1109,476),[389];(1109,478),[409];(1109,479),[413];(1109,480),[417];(1109,483),[419];(1109,484),[423];(1109,486),[425];(1109,488),[429];(1109,489),[433];(1109,491),[435];(1109,493),[437];(1109,512),[512];(1109,585),[728];(1109,586),[726];(1109,587),[724];(1109,588),[730];(1109,622),[861];(1109,650),[214];(1109,652),[212];(1109,653),[223];(1109,654),[221];(1109,655),[229;230];(1109,656),[232];(1109,657),[239];(1109,658),[242];(1109,659),[245];(1109,660),[254;255];(1109,661),[249];(1109,663),[247];(1109,664),[253];(1109,665),[251];(1109,666),[258;259];(1109,667),[257];(1109,668),[261];(1109,669),[266;267];(1109,670),[265];(1109,671),[263];(1109,672),[270];(1109,674),[272];(1109,675),[276];(1109,676),[274];(1109,679),[291;292];(1109,681),[300];(1109,682),[306];(1109,683),[302];(1109,685),[304];(1109,686),[308];(1109,687),[309;310];(1109,688),[312];(1109,689),[318];(1109,690),[314];(1109,691),[316];(1109,692),[320];(1109,694),[330];(1109,696),[334];(1109,709),[150];(1109,713),[215];(1109,714),[224];(1109,717),[293];(1109,718),[322];(1109,722),[390];(1109,732),[682];(1109,738),[152];(1109,742),[217];(1109,743),[226];(1109,746),[295];(1109,747),[324];(1109,750),[392];(1109,760),[684];(1109,768),[154];(1109,772),[219];(1109,773),[228];(1109,776),[297];(1109,777),[326];(1109,780),[394];(1109,790),[686];(1109,797),[12];(1110,3),[843];(1110,12),[268];(1110,32),[240];(1110,34),[387];(1110,67),[321];(1110,153),[1021];(1110,155),[1023];(1110,159),[1028];(1110,170),[1055];(1110,454),[885;886];(1110,455),[405];(1110,456),[370];(1110,471),[378];(1110,476),[389];(1110,478),[409];(1110,479),[413];(1110,480),[417];(1110,483),[419];(1110,484),[423];(1110,486),[425];(1110,488),[429];(1110,489),[433];(1110,491),[435];(1110,493),[437];(1110,512),[512];(1110,585),[728];(1110,586),[726];(1110,587),[724];(1110,588),[730];(1110,622),[861];(1110,650),[214];(1110,652),[212];(1110,653),[223];(1110,654),[221];(1110,655),[229;230];(1110,656),[232];(1110,657),[239];(1110,658),[242];(1110,659),[245];(1110,660),[254;255];(1110,661),[249];(1110,663),[247];(1110,664),[253];(1110,665),[251];(1110,666),[258;259];(1110,667),[257];(1110,668),[261];(1110,669),[266;267];(1110,670),[265];(1110,671),[263];(1110,672),[270];(1110,674),[272];(1110,675),[276];(1110,676),[274];(1110,679),[291;292];(1110,681),[300];(1110,682),[306];(1110,683),[302];(1110,685),[304];(1110,686),[308];(1110,687),[309;310];(1110,688),[312];(1110,689),[318];(1110,690),[314];(1110,691),[316];(1110,692),[320];(1110,694),[330];(1110,696),[334];(1110,709),[150];(1110,713),[215];(1110,714),[224];(1110,717),[293];(1110,718),[322];(1110,722),[390];(1110,732),[682];(1110,738),[152];(1110,742),[217];(1110,743),[226];(1110,746),[295];(1110,747),[324];(1110,750),[392];(1110,760),[684];(1110,768),[154];(1110,772),[219];(1110,773),[228];(1110,776),[297];(1110,777),[326];(1110,780),[394];(1110,790),[686];(1110,797),[12];(1111,3),[852];(1111,12),[268];(1111,24),[703];(1111,32),[240];(1111,34),[387];(1111,43),[514;515];(1111,51),[373;374;375;376];(1111,66),[364];(1111,67),[321];(1111,73),[873];(1111,79),[399];(1111,153),[1021];(1111,155),[1023];(1111,159),[1028];(1111,170),[1055];(1111,181),[1069];(1111,193),[1091];(1111,195),[1094];(1111,204),[1105];(1111,303),[1258];(1111,308),[1266];(1111,309),[1268];(1111,321),[1286];(1111,328),[1295];(1111,353),[1337];(1111,355),[1341];(1111,381),[1397];(1111,382),[1400];(1111,385),[1407];(1111,387),[1411];(1111,419),[1479];(1111,420),[1482];(1111,421),[1484];(1111,424),[1488];(1111,425),[1491];(1111,433),[1505];(1111,437),[1517];(1111,444),[1534];(1111,454),[885;886];(1111,455),[405];(1111,456),[370];(1111,471),[378];(1111,476),[389];(1111,478),[409];(1111,479),[413];(1111,480),[417];(1111,483),[419];(1111,484),[423];(1111,486),[425];(1111,488),[429];(1111,489),[433];(1111,491),[435];(1111,493),[437];(1111,512),[512];(1111,517),[532];(1111,543),[606];(1111,555),[629];(1111,564),[660];(1111,577),[712];(1111,585),[728];(1111,586),[726];(1111,587),[724];(1111,588),[730];(1111,594),[757];(1111,595),[764];(1111,596),[759];(1111,600),[769];(1111,622),[861];(1111,635),[171];(1111,636),[173];(1111,637),[175];(1111,650),[214];(1111,652),[212];(1111,653),[223];(1111,654),[221];(1111,655),[229;230];(1111,656),[232];(1111,657),[239];(1111,658),[242];(1111,659),[245];(1111,660),[254;255];(1111,661),[249];(1111,663),[247];(1111,664),[253];(1111,665),[251];(1111,666),[258;259];(1111,667),[257];(1111,668),[261];(1111,669),[266;267];(1111,670),[265];(1111,671),[263];(1111,672),[270];(1111,674),[272];(1111,675),[276];(1111,676),[274];(1111,679),[291;292];(1111,681),[300];(1111,682),[306];(1111,683),[302];(1111,685),[304];(1111,686),[308];(1111,687),[309;310];(1111,688),[312];(1111,689),[318];(1111,690),[314];(1111,691),[316];(1111,692),[320];(1111,694),[330];(1111,696),[334];(1111,698),[338];(1111,700),[348];(1111,704),[366];(1111,705),[368];(1111,708),[741];(1111,709),[150];(1111,713),[215];(1111,714),[224];(1111,717),[293];(1111,718),[322];(1111,721),[359];(1111,722),[390];(1111,732),[682];(1111,733),[691];(1111,734),[704];(1111,735),[742];(1111,736),[747];(1111,737),[894];(1111,738),[151;152];(1111,742),[217];(1111,743),[226];(1111,746),[295];(1111,747),[323;324];(1111,749),[360];(1111,750),[391;392];(1111,760),[684];(1111,761),[692];(1111,763),[705];(1111,764),[743];(1111,765),[748];(1111,766),[895];(1111,768),[154];(1111,772),[219];(1111,773),[228];(1111,776),[297];(1111,777),[326];(1111,780),[394];(1111,790),[686];(1111,797),[12];(1111,802),[528;529];(1112,3),[838];(1112,12),[268];(1112,24),[703];(1112,32),[240];(1112,34),[387];(1112,43),[514;515];(1112,51),[373;374;375;376];(1112,66),[364];(1112,67),[321];(1112,73),[873];(1112,79),[399];(1112,153),[1021];(1112,155),[1023];(1112,159),[1028];(1112,170),[1055];(1112,181),[1069];(1112,193),[1091];(1112,195),[1094];(1112,204),[1105];(1112,235),[1148];(1112,236),[1149];(1112,303),[1258];(1112,308),[1266];(1112,309),[1268];(1112,321),[1286];(1112,328),[1295];(1112,353),[1335];(1112,355),[1341];(1112,381),[1397];(1112,382),[1400];(1112,385),[1407];(1112,387),[1411];(1112,397),[1432];(1112,399),[1436];(1112,419),[1479];(1112,420),[1482];(1112,421),[1484];(1112,424),[1488];(1112,425),[1491];(1112,433),[1505];(1112,437),[1517];(1112,444),[1534];(1112,454),[885;886];(1112,455),[405];(1112,456),[370];(1112,471),[378];(1112,476),[389];(1112,478),[409];(1112,479),[413];(1112,480),[417];(1112,483),[419];(1112,484),[423];(1112,486),[425];(1112,488),[429];(1112,489),[433];(1112,491),[435];(1112,493),[437];(1112,512),[512];(1112,517),[532];(1112,543),[606];(1112,555),[629];(1112,564),[660];(1112,577),[712];(1112,585),[728];(1112,586),[726];(1112,587),[724];(1112,588),[730];(1112,594),[757];(1112,595),[764];(1112,596),[759];(1112,600),[769];(1112,622),[861];(1112,635),[171];(1112,636),[173];(1112,637),[175];(1112,650),[214];(1112,652),[212];(1112,653),[223];(1112,654),[221];(1112,655),[229;230];(1112,656),[232];(1112,657),[239];(1112,658),[242];(1112,659),[245];(1112,660),[254;255];(1112,661),[249];(1112,663),[247];(1112,664),[253];(1112,665),[251];(1112,666),[258;259];(1112,667),[257];(1112,668),[261];(1112,669),[266;267];(1112,670),[265];(1112,671),[263];(1112,672),[270];(1112,674),[272];(1112,675),[276];(1112,676),[274];(1112,679),[291;292];(1112,681),[300];(1112,682),[306];(1112,683),[302];(1112,685),[304];(1112,686),[308];(1112,687),[309;310];(1112,688),[312];(1112,689),[318];(1112,690),[314];(1112,691),[316];(1112,692),[320];(1112,694),[330];(1112,696),[334];(1112,698),[338];(1112,700),[348];(1112,704),[366];(1112,705),[368];(1112,708),[741];(1112,709),[150];(1112,713),[215];(1112,714),[224];(1112,717),[293];(1112,718),[322];(1112,721),[359];(1112,722),[390];(1112,732),[682];(1112,733),[691];(1112,734),[704];(1112,735),[742];(1112,736),[747];(1112,737),[894];(1112,738),[151;152];(1112,742),[217];(1112,743),[226];(1112,746),[295];(1112,747),[323;324];(1112,749),[360];(1112,750),[391;392];(1112,760),[684];(1112,761),[692];(1112,763),[705];(1112,764),[743];(1112,765),[748];(1112,766),[895];(1112,768),[154];(1112,772),[219];(1112,773),[228];(1112,776),[297];(1112,777),[326];(1112,780),[394];(1112,790),[686];(1112,797),[12];(1112,801),[493;494];(1112,802),[528;529];(1113,3),[846];(1113,12),[268];(1113,32),[240];(1113,34),[387];(1113,67),[321];(1113,153),[1021];(1113,155),[1023];(1113,159),[1028];(1113,170),[1055];(1113,454),[885;886];(1113,455),[405];(1113,456),[370];(1113,471),[378];(1113,476),[389];(1113,478),[409];(1113,479),[413];(1113,480),[417];(1113,483),[419];(1113,484),[423];(1113,486),[425];(1113,488),[429];(1113,489),[433];(1113,491),[435];(1113,493),[437];(1113,512),[512];(1113,585),[728];(1113,586),[726];(1113,587),[724];(1113,588),[730];(1113,622),[861];(1113,650),[214];(1113,652),[212];(1113,653),[223];(1113,654),[221];(1113,655),[229;230];(1113,656),[232];(1113,657),[239];(1113,658),[242];(1113,659),[245];(1113,660),[254;255];(1113,661),[249];(1113,663),[247];(1113,664),[253];(1113,665),[251];(1113,666),[258;259];(1113,667),[257];(1113,668),[261];(1113,669),[266;267];(1113,670),[265];(1113,671),[263];(1113,672),[270];(1113,674),[272];(1113,675),[276];(1113,676),[274];(1113,679),[291;292];(1113,681),[300];(1113,682),[306];(1113,683),[302];(1113,685),[304];(1113,686),[308];(1113,687),[309;310];(1113,688),[312];(1113,689),[318];(1113,690),[314];(1113,691),[316];(1113,692),[320];(1113,694),[330];(1113,696),[334];(1113,709),[150];(1113,713),[215];(1113,714),[224];(1113,717),[293];(1113,718),[322];(1113,722),[390];(1113,732),[682];(1113,738),[152];(1113,742),[217];(1113,743),[226];(1113,746),[295];(1113,747),[324];(1113,750),[392];(1113,760),[684];(1113,768),[154];(1113,772),[219];(1113,773),[228];(1113,776),[297];(1113,777),[326];(1113,780),[394];(1113,790),[686];(1113,797),[12];(1114,601),[772];(1114,605),[782];(1114,606),[783];(1116,4),[450];(1116,7),[196];(1116,10),[561];(1116,12),[268];(1116,32),[240];(1116,34),[387];(1116,48),[354];(1116,67),[321];(1116,127),[987];(1116,128),[989;990];(1116,153),[1021];(1116,155),[1023];(1116,159),[1028];(1116,170),[1055];(1116,247),[1179];(1116,328),[1295];(1116,404),[1447];(1116,425),[1489];(1116,454),[886];(1116,455),[405];(1116,456),[370];(1116,457),[411];(1116,459),[580];(1116,460),[602];(1116,461),[625];(1116,462),[656];(1116,463),[677];(1116,467),[182];(1116,468),[351];(1116,471),[378];(1116,476),[389];(1116,478),[409];(1116,479),[413];(1116,480),[417];(1116,483),[419];(1116,484),[423];(1116,486),[425];(1116,488),[429];(1116,489),[433];(1116,491),[435];(1116,493),[437];(1116,496),[448;449];(1116,497),[452];(1116,498),[454];(1116,499),[456];(1116,500),[458];(1116,501),[460];(1116,502),[462];(1116,504),[472];(1116,505),[474];(1116,506),[477];(1116,507),[479];(1116,508),[502];(1116,511),[510];(1116,512),[512];(1116,513),[88];(1116,524),[90];(1116,526),[564];(1116,527),[566];(1116,528),[570];(1116,530),[576];(1116,531),[572];(1116,532),[574];(1116,533),[578];(1116,535),[92];(1116,536),[585];(1116,537),[587];(1116,538),[594];(1116,541),[600];(1116,542),[604];(1116,544),[611];(1116,545),[609];(1116,546),[94];(1116,547),[631];(1116,549),[615];(1116,550),[617];(1116,551),[619];(1116,553),[623];(1116,554),[627];(1116,557),[96];(1116,559),[648];(1116,560),[650];(1116,561),[652];(1116,562),[654];(1116,563),[658];(1116,568),[98];(1116,585),[728];(1116,586),[726];(1116,587),[724];(1116,588),[730];(1116,595),[764;765];(1116,596),[759];(1116,597),[761];(1116,598),[763];(1116,601),[773];(1116,605),[782];(1116,606),[784];(1116,607),[792];(1116,608),[788];(1116,612),[820];(1116,614),[822];(1116,615),[824];(1116,616),[826];(1116,617),[828];(1116,618),[830];(1116,622),[861];(1116,632),[149];(1116,641),[185];(1116,642),[190;191];(1116,643),[187];(1116,644),[189];(1116,645),[193];(1116,646),[194;195];(1116,647),[198];(1116,648),[200];(1116,650),[214];(1116,652),[212];(1116,653),[223];(1116,654),[221];(1116,655),[229;230];(1116,656),[232];(1116,657),[239];(1116,658),[242];(1116,659),[245];(1116,660),[254;255];(1116,661),[249];(1116,663),[247];(1116,664),[253];(1116,665),[251];(1116,666),[258;259];(1116,667),[257];(1116,668),[261];(1116,669),[266;267];(1116,670),[265];(1116,671),[263];(1116,672),[270];(1116,674),[272];(1116,675),[276];(1116,676),[274];(1116,679),[291;292];(1116,681),[300];(1116,682),[306];(1116,683),[302];(1116,685),[304];(1116,686),[308];(1116,687),[309;310];(1116,688),[312];(1116,689),[318];(1116,690),[314];(1116,691),[316];(1116,692),[320];(1116,694),[330];(1116,696),[334];(1116,699),[346];(1116,700),[349];(1116,701),[352;353];(1116,702),[358];(1116,703),[356];(1116,709),[150];(1116,713),[215];(1116,714),[224];(1116,715),[233];(1116,716),[278];(1116,717),[293];(1116,718),[322];(1116,719),[57];(1116,720),[340];(1116,721),[359];(1116,722),[390];(1116,723),[466];(1116,724),[487];(1116,725),[496];(1116,726),[544];(1116,727),[539];(1116,728),[588];(1116,729),[633];(1116,730),[74];(1116,731),[638];(1116,732),[682];(1116,733),[691];(1116,735),[742];(1116,736),[747];(1116,737),[894];(1116,738),[152];(1116,742),[217];(1116,743),[226];(1116,744),[235];(1116,745),[280];(1116,746),[295];(1116,747),[324];(1116,748),[341;342];(1116,749),[361];(1116,750),[392];(1116,751),[59];(1116,752),[468];(1116,753),[489];(1116,754),[498];(1116,755),[545;546];(1116,756),[541];(1116,757),[590];(1116,758),[635];(1116,759),[640];(1116,760),[684];(1116,761),[693];(1116,762),[76];(1116,764),[743;744];(1116,765),[749];(1116,766),[896];(1116,768),[154];(1116,772),[219];(1116,773),[228];(1116,774),[237];(1116,775),[282];(1116,776),[297];(1116,777),[326];(1116,778),[344];(1116,779),[363];(1116,780),[394];(1116,781),[470];(1116,782),[491];(1116,783),[500];(1116,784),[61];(1116,785),[548];(1116,786),[543];(1116,787),[592];(1116,788),[637];(1116,789),[642];(1116,790),[686];(1116,791),[695];(1116,793),[746];(1116,794),[751];(1116,795),[78];(1116,796),[898];(1116,797),[12];(1117,12),[268];(1117,32),[240];(1117,34),[387];(1117,67),[321];(1117,107),[958];(1117,153),[1021];(1117,155),[1023];(1117,159),[1028];(1117,170),[1055];(1117,296),[1249];(1117,325),[1291];(1117,328),[1295];(1117,425),[1489];(1117,454),[886];(1117,455),[405];(1117,456),[370];(1117,465),[115];(1117,471),[378];(1117,476),[389];(1117,478),[409];(1117,479),[413];(1117,480),[417];(1117,481),[80];(1117,483),[419];(1117,484),[423];(1117,486),[425];(1117,488),[429];(1117,489),[433];(1117,491),[435];(1117,492),[73];(1117,493),[437];(1117,512),[512];(1117,522),[556];(1117,523),[558];(1117,525),[559;560];(1117,570),[679];(1117,571),[681];(1117,572),[688];(1117,573),[690];(1117,574),[696;697];(1117,575),[700];(1117,576),[702];(1117,577),[712];(1117,581),[716];(1117,585),[728];(1117,586),[726];(1117,587),[724];(1117,588),[729;730];(1117,589),[733];(1117,590),[734;735];(1117,591),[104];(1117,594),[756;757];(1117,595),[764;765];(1117,596),[759];(1117,597),[761];(1117,598),[763];(1117,599),[767];(1117,603),[777];(1117,604),[778;779];(1117,609),[794];(1117,610),[796];(1117,611),[797;798];(1117,612),[820];(1117,614),[822];(1117,615),[824];(1117,616),[826];(1117,617),[828];(1117,618),[830];(1117,622),[861];(1117,624),[112;113];(1117,627),[116;117];(1117,628),[136;137];(1117,629),[48];(1117,630),[140];(1117,631),[146];(1117,633),[156];(1117,639),[178;179];(1117,640),[50];(1117,650),[214];(1117,651),[51;52];(1117,652),[212];(1117,653),[223];(1117,654),[221];(1117,655),[229;230];(1117,656),[232];(1117,657),[238;239];(1117,658),[242];(1117,659),[245];(1117,660),[254;255];(1117,661),[249];(1117,663),[247];(1117,664),[253];(1117,665),[251];(1117,666),[258;259];(1117,667),[257];(1117,668),[261];(1117,669),[266;267];(1117,670),[265];(1117,671),[263];(1117,672),[270];(1117,674),[272];(1117,675),[276];(1117,676),[274];(1117,678),[286];(1117,679),[291;292];(1117,681),[300];(1117,682),[306];(1117,683),[302];(1117,685),[304];(1117,686),[308];(1117,687),[309;310];(1117,688),[312];(1117,689),[318];(1117,690),[314];(1117,691),[316];(1117,692),[320];(1117,694),[330];(1117,696),[334];(1117,708),[741];(1117,709),[150];(1117,710),[157];(1117,711),[163];(1117,713),[215];(1117,714),[224];(1117,717),[293];(1117,718),[322];(1117,722),[390];(1117,730),[74];(1117,732),[682];(1117,734),[704];(1117,735),[742];(1117,738),[152];(1117,739),[159];(1117,740),[165];(1117,742),[217];(1117,743),[226];(1117,746),[295];(1117,747),[324];(1117,750),[392];(1117,760),[684];(1117,762),[76];(1117,763),[706];(1117,764),[743;744];(1117,768),[154];(1117,769),[161];(1117,770),[167];(1117,772),[219];(1117,773),[228];(1117,776),[297];(1117,777),[326];(1117,780),[394];(1117,790),[686];(1117,792),[708];(1117,793),[746];(1117,795),[78];(1117,797),[12];(1118,3),[841];(1118,12),[268];(1118,24),[703];(1118,32),[240];(1118,34),[387];(1118,43),[514;515];(1118,51),[373;374;375;376];(1118,66),[364];(1118,67),[321];(1118,73),[870];(1118,79),[399];(1118,153),[1021];(1118,155),[1023];(1118,159),[1028];(1118,170),[1055];(1118,181),[1069;1071];(1118,193),[1091];(1118,195),[1094];(1118,204),[1105];(1118,223),[1126];(1118,227),[1132];(1118,303),[1258];(1118,308),[1266];(1118,309),[1268];(1118,321),[1286];(1118,328),[1295];(1118,355),[1341];(1118,381),[1397];(1118,382),[1400;1402];(1118,385),[1407];(1118,387),[1411];(1118,419),[1479];(1118,420),[1482];(1118,421),[1484];(1118,424),[1488];(1118,425),[1491];(1118,433),[1505];(1118,437),[1517];(1118,444),[1534];(1118,454),[885;886];(1118,455),[405];(1118,456),[370];(1118,471),[378];(1118,476),[389];(1118,478),[409];(1118,479),[413];(1118,480),[417];(1118,483),[419];(1118,484),[423];(1118,486),[425];(1118,488),[429];(1118,489),[433];(1118,491),[435];(1118,493),[437];(1118,512),[512];(1118,517),[532];(1118,543),[606];(1118,555),[629];(1118,564),[660];(1118,577),[712];(1118,585),[728];(1118,586),[726];(1118,587),[724];(1118,588),[730];(1118,594),[757];(1118,595),[764];(1118,596),[759];(1118,600),[769];(1118,622),[861];(1118,635),[171];(1118,636),[173];(1118,637),[175];(1118,650),[214];(1118,652),[212];(1118,653),[223];(1118,654),[221];(1118,655),[229;230];(1118,656),[232];(1118,657),[239];(1118,658),[242];(1118,659),[245];(1118,660),[254;255];(1118,661),[249];(1118,663),[247];(1118,664),[253];(1118,665),[251];(1118,666),[258;259];(1118,667),[257];(1118,668),[261];(1118,669),[266;267];(1118,670),[265];(1118,671),[263];(1118,672),[270];(1118,674),[272];(1118,675),[276];(1118,676),[274];(1118,679),[291;292];(1118,681),[300];(1118,682),[306];(1118,683),[302];(1118,685),[304];(1118,686),[308];(1118,687),[309;310];(1118,688),[312];(1118,689),[318];(1118,690),[314];(1118,691),[316];(1118,692),[320];(1118,694),[330];(1118,696),[334];(1118,698),[338];(1118,700),[348];(1118,704),[366];(1118,705),[368];(1118,708),[741];(1118,709),[150];(1118,713),[215];(1118,714),[224];(1118,717),[293];(1118,718),[322];(1118,721),[359];(1118,722),[390];(1118,732),[682];(1118,733),[691];(1118,734),[704];(1118,735),[742];(1118,736),[747];(1118,737),[894];(1118,738),[151;152];(1118,742),[217];(1118,743),[226];(1118,746),[295];(1118,747),[323;324];(1118,749),[360];(1118,750),[391;392];(1118,760),[684];(1118,761),[692];(1118,763),[705];(1118,764),[743];(1118,765),[748];(1118,766),[895];(1118,768),[154];(1118,772),[219];(1118,773),[228];(1118,776),[297];(1118,777),[326];(1118,780),[394];(1118,790),[686];(1118,797),[12];(1118,802),[528;529];(1119,12),[268];(1119,17),[770];(1119,29),[739];(1119,30),[738];(1119,32),[240];(1119,34),[387];(1119,54),[132];(1119,67),[321];(1119,110),[961];(1119,111),[962];(1119,153),[1021];(1119,155),[1023];(1119,159),[1028];(1119,170),[1055];(1119,296),[1249];(1119,319),[1283];(1119,328),[1295];(1119,425),[1489];(1119,454),[886];(1119,455),[405];(1119,456),[370];(1119,465),[114;115];(1119,466),[142];(1119,471),[378];(1119,476),[389];(1119,478),[409];(1119,479),[413];(1119,480),[417];(1119,481),[80];(1119,483),[419];(1119,484),[423];(1119,486),[425];(1119,488),[429];(1119,489),[433];(1119,491),[435];(1119,492),[73];(1119,493),[437];(1119,512),[512];(1119,522),[556];(1119,523),[558];(1119,525),[560];(1119,570),[679];(1119,571),[681];(1119,572),[688];(1119,573),[690];(1119,574),[697];(1119,575),[700];(1119,576),[702];(1119,577),[712];(1119,581),[716];(1119,585),[728];(1119,586),[726];(1119,587),[724];(1119,588),[730];(1119,589),[733];(1119,590),[735];(1119,592),[753];(1119,594),[757];(1119,595),[764;765];(1119,596),[759];(1119,597),[761];(1119,598),[763];(1119,599),[767];(1119,603),[777];(1119,604),[779];(1119,609),[794];(1119,610),[796];(1119,611),[798];(1119,612),[820];(1119,614),[822];(1119,615),[824];(1119,616),[826];(1119,617),[828];(1119,618),[830];(1119,622),[861];(1119,624),[113];(1119,627),[117];(1119,628),[137];(1119,629),[48];(1119,630),[141];(1119,631),[146];(1119,633),[156];(1119,639),[179];(1119,640),[50];(1119,650),[214];(1119,651),[52];(1119,652),[212];(1119,653),[223];(1119,654),[221];(1119,655),[229;230];(1119,656),[232];(1119,657),[239];(1119,658),[242];(1119,659),[245];(1119,660),[254;255];(1119,661),[249];(1119,663),[247];(1119,664),[253];(1119,665),[251];(1119,666),[258;259];(1119,667),[257];(1119,668),[261];(1119,669),[266;267];(1119,670),[265];(1119,671),[263];(1119,672),[270];(1119,674),[272];(1119,675),[276];(1119,676),[274];(1119,679),[291;292];(1119,681),[300];(1119,682),[306];(1119,683),[302];(1119,685),[304];(1119,686),[308];(1119,687),[309;310];(1119,688),[312];(1119,689),[318];(1119,690),[314];(1119,691),[316];(1119,692),[320];(1119,694),[330];(1119,696),[334];(1119,708),[741];(1119,709),[150];(1119,710),[157];(1119,711),[163];(1119,713),[215];(1119,714),[224];(1119,717),[293];(1119,718),[322];(1119,722),[390];(1119,730),[74];(1119,732),[682];(1119,734),[704];(1119,735),[742];(1119,738),[152];(1119,739),[159];(1119,740),[165];(1119,742),[217];(1119,743),[226];(1119,746),[295];(1119,747),[324];(1119,750),[392];(1119,760),[684];(1119,762),[76];(1119,763),[706];(1119,764),[743;744];(1119,768),[154];(1119,769),[161];(1119,770),[167];(1119,772),[219];(1119,773),[228];(1119,776),[297];(1119,777),[326];(1119,780),[394];(1119,790),[686];(1119,792),[708];(1119,793),[746];(1119,795),[78];(1119,797),[12];(1120,12),[268];(1120,15),[481];(1120,24),[703];(1120,32),[240];(1120,34),[387];(1120,43),[514;515];(1120,51),[373;374;375;376];(1120,52),[485];(1120,56),[492];(1120,66),[364];(1120,67),[321];(1120,71),[484];(1120,73),[879];(1120,74),[5];(1120,79),[399];(1120,88),[927];(1120,113),[965];(1120,153),[1021];(1120,155),[1023];(1120,159),[1028];(1120,170),[1055];(1120,181),[1069];(1120,193),[1091];(1120,195),[1094];(1120,198),[1099];(1120,204),[1105];(1120,214),[1117];(1120,230),[1138;1140;1141];(1120,234),[1146;1147];(1120,237),[1150];(1120,303),[1258];(1120,308),[1266];(1120,309),[1268];(1120,321),[1286];(1120,328),[1295];(1120,332),[1300];(1120,333),[1301];(1120,336),[1305];(1120,337),[1307];(1120,355),[1341];(1120,361),[1353];(1120,381),[1397];(1120,382),[1400];(1120,384),[1406];(1120,385),[1407];(1120,387),[1411];(1120,388),[1412];(1120,389),[1414];(1120,391),[1417;1418];(1120,392),[1419];(1120,394),[1426];(1120,395),[1428];(1120,398),[1434;1435];(1120,400),[1438;1439];(1120,401),[1440;1442;1443];(1120,402),[1444];(1120,419),[1479];(1120,420),[1482];(1120,421),[1484];(1120,424),[1488];(1120,425),[1491];(1120,426),[1492];(1120,427),[1494];(1120,433),[1505];(1120,437),[1517];(1120,438),[1520];(1120,442),[1527;1529;1530];(1120,444),[1534];(1120,454),[886];(1120,455),[405];(1120,456),[370];(1120,471),[378];(1120,476),[389];(1120,478),[409];(1120,479),[413];(1120,480),[417];(1120,483),[419];(1120,484),[423];(1120,486),[425];(1120,488),[429];(1120,489),[433];(1120,491),[435];(1120,493),[437];(1120,512),[512];(1120,517),[532];(1120,543),[606];(1120,555),[629];(1120,564),[660];(1120,577),[712];(1120,585),[728];(1120,586),[726];(1120,587),[723;724];(1120,588),[730];(1120,594),[757];(1120,595),[764];(1120,596),[759];(1120,600),[769];(1120,601),[773];(1120,605),[782];(1120,606),[784];(1120,622),[861];(1120,635),[171];(1120,636),[173];(1120,637),[175];(1120,650),[214];(1120,652),[212];(1120,653),[223];(1120,654),[221];(1120,655),[229;230];(1120,656),[232];(1120,657),[239];(1120,658),[242];(1120,659),[245];(1120,660),[254;255];(1120,661),[249];(1120,663),[247];(1120,664),[253];(1120,665),[251];(1120,666),[258;259];(1120,667),[257];(1120,668),[261];(1120,669),[266;267];(1120,670),[265];(1120,671),[263];(1120,672),[270];(1120,674),[272];(1120,675),[276];(1120,676),[274];(1120,679),[291;292];(1120,681),[300];(1120,682),[306];(1120,683),[302];(1120,685),[304];(1120,686),[308];(1120,687),[309;310];(1120,688),[312];(1120,689),[318];(1120,690),[314];(1120,691),[316];(1120,692),[320];(1120,694),[330];(1120,696),[334];(1120,698),[338];(1120,700),[348];(1120,704),[366];(1120,705),[368];(1120,708),[741];(1120,709),[150];(1120,713),[215];(1120,714),[224];(1120,717),[293];(1120,718),[322];(1120,721),[359];(1120,722),[390];(1120,724),[487];(1120,725),[496];(1120,732),[682];(1120,733),[691];(1120,734),[704];(1120,735),[742];(1120,736),[747];(1120,737),[894];(1120,738),[151;152];(1120,742),[217];(1120,743),[226];(1120,746),[295];(1120,747),[323;324];(1120,749),[360];(1120,750),[391;392];(1120,753),[488];(1120,754),[497];(1120,760),[684];(1120,761),[692];(1120,763),[705];(1120,764),[743];(1120,765),[748];(1120,766),[895];(1120,768),[154];(1120,772),[219];(1120,773),[228];(1120,776),[297];(1120,777),[326];(1120,780),[394];(1120,790),[686];(1120,797),[12];(1120,800),[482;483];(1120,802),[528;529];(1120,803),[774;775];(1120,804),[785;786];(1121,239),[1152];(1121,508),[501]|] 
let parse tokens = ParserBase(806, 812, actions, productions, (Seq.map tokenToNumber tokens)).parse()
