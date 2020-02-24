-module(server).
-export([start/0]).
-export([peer_up/3, peer_down/3]).

start() ->
	diameter:start(),
	diameter:start_service(
		server,
		[
		  {'Origin-Host', "server.local"},
			{'Origin-Realm', "local"},
			{'Vendor-Id', 0},
			{'Product-Name', "Server"},
			{'Auth-Application-Id', [0]},
			{
				application,
				[
					{dictionary, diameter_gen_base_rfc6733},
					{module, server}
				]
			}
		]
	),
	diameter:add_transport(
		server,
		{
			listen,
			[
				{transport_module, diameter_tcp},
				{transport_config,
					[
						{reuseaddr, true},
						{ip, {127,0,0,1}},
						{port, 3868}
					]
				}
			]
		}
	).

peer_up(_SvcName, _Peer, State) ->
    State.
peer_down(_SvcName, _Peer, State) ->
    State.
