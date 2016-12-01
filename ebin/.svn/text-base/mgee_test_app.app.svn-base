{application, mgee_test_app,
 [{description, "Ming Game Engine Erlang - Simulate Stress Test"},
  {id, "MgeeTestApp"},
  {vsn, "1.0.0"},
  {modules, []},
  {registered, [mgee_test_sup]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {mgee_test_app, []}},
  {env, [
  		{ log_path, "mgee_test.log"},
  		{ host, "192.168.1.51"},
    	{ port, 8888 },
    	{ process_count , 2 },
    	{ timeout, 2000 },
    	{ speed, 2 }    
  		]}
  ]}.