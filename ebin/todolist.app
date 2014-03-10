{application, todolist, [
    {description, "A todolist application"},
    {vsn, "1.0"},
    {modules, [
        todolist, 
        todolist_sup, 
        task_srv
    ]},
    {registered, []},
    {applications, [
        kernel,
        stdlib
    ]},
    {env, []},
    {mod, {todolist, []}}
    ]
}.
