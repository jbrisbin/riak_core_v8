-ifndef(DYNJS).
-define(DYNJS, ok).

-define(VNODE_MASTER(Name, VNode), {Name, {riak_core_vnode_master, start_link, [VNode]}, permanent, 5000, worker, [riak_core_vnode_master]}).
-define(GEN_SERVER(Name, Worker), {Name, {Worker, start_link, []}, permanent, 5000, worker, [Worker]}).

-endif.