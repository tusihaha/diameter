-module(client).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

-export([start/0, call/0]).
-export([peer_up/3, peer_down/3, pick_peer/4]).
-export([prepare_request/3]).

-define(DEF_SVC_NAME, ?MODULE).
-define(L, atom_to_list).

start() ->
  diameter:start_service(
		client,
		[
      {'Origin-Host', "client.local"},
      {'Origin-Realm', "local"},
      {'Vendor-Id', 0},
      {'Product-Name', "Client"},
      {'Auth-Application-Id', [0]},
			{
				application,
				[
          {alias, common},
					{dictionary, diameter_gen_base_rfc6733},
					{module, client}
				]
			}
		]
	),
  diameter:add_transport(
    client,
    {
			connect,
			[
				{transport_module, diameter_tcp},
				{transport_config,
					[
            {reuseaddr, true},
						{raddr, {127,0,0,1}},
            {rport, 3868}
					]
				}
			]
		}
  ).

call(Name) ->
    SId = diameter:session_id(?L(Name)),
    RAR = ['RAR' | [{'Session-Id', SId}, {'Auth-Application-Id', 0}, {'Re-Auth-Request-Type', 0}]],
    diameter:call(Name, common, RAR, []).

call() ->
    call(?DEF_SVC_NAME).


prepare_request(#diameter_packet{msg = ['RAR' = T | Avps]}, _, {_, Caps}) ->
    #diameter_caps{origin_host = {OH, DH},
                   origin_realm = {OR, DR}}
        = Caps,

    {send, [T | if is_map(Avps) ->
                        Avps#{'Origin-Host' => OH,
                              'Origin-Realm' => OR,
                              'Destination-Host' => DH,
                              'Destination-Realm' => DR};
                   is_list(Avps) ->
                        [{'Origin-Host', OH},
                         {'Origin-Realm', OR},
                         {'Destination-Host', DH},
                         {'Destination-Realm', DR}
                         | Avps]
                end]}.

peer_up(_SvcName, _Peer, State) ->
    State.
peer_down(_SvcName, _Peer, State) ->
    State.
pick_peer([Peer | _], _, _SvcName, _State) ->
  {ok, Peer}.
