{application, data_sync,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [ds_dir_app, ds_dir_sup, ds_dir]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { data_sync_app, []}},
  {env, []}
 ]}.
