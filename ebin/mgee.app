{application, mgee,
 [{description, "Ming game engine write with erlang"},
  {id, "Mgee"},
  {vsn, "1.0.0"},
  {modules, []},
  {registered, [mgee_sup]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {mgee, []}},
  {env, [	{tcp_listeners, [{"192.168.80.129", 3333}]},
  			{acceptor_num, 10},
  			{log_path, "mgee.log"},
  			{config_path, "../config"},
  			{test_mode, true}
  			]}
  ]}.






