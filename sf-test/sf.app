{application, sf,
  [{description, "A sendfile test"},
   {vsn, "0.0.1"},
   {modules, [sf]},
   {registered, [sf_sup]},
   {applications, [kernel, stdlib]},
   {mod, {sf_app, []}}
  ]}.