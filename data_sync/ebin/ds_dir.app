{application, ds_dir,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [ds_dir_app, ds_dir_sup, ds_dir]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { ds_dir_app, []}},
  {env, []}
 ]}.
