module m1(sm1, sm2)
[imports
	logger,
	timer
]
[out
	b,
	Current
]
[in
	fun/1 do_add
]
{
	int timeout = 5000;
	int[] queue = [];
	timer t = new timer(timeout);
	logger log = new logger('semafor.log');
	int a = 0;
	int b = 0;

	b <~ a + 1;

	fun/1 do_add = fun(item)
	{
		queue.add(item);
		!add;
	}
	
	event add 
	<-	add_handler
	<-	handler
		{
			log.log("added: ", changes);
		}.
	
	handler add_handler = 
		handler
		{
			queue.add(changes[0]);
		}.
	
	event add_to_empty 
	[on 
		add, 
		queue.length == 0
	]. 
	<-	fun()
		{
			t.start();
		}.
	
	event 
	[on 
		t.alert
	]. 
	<-	handler
		{
			queue.pop;
			t.start;
		}. 
	<-	handler
		{
			log.log("in: " + queue[0]);
			a = a + 1;
		}.
	
	Current = queue[0];
}.

module main
[imports
	m1
];
[body
	semafor sem = new semafor();
	
	load
	<-	handler
		{
			sem.add(1);
			sem.add(2);
			sem.add(2);
			sem.add(3);
		}.
	
	event third_in 
	[on 
		sem.Current == 3
	]	 
	<-	handler
		{
			sem.add(1);
		}.

].