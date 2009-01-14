{application, www_jabber_se, [
	{description,  "Jabber.se"},
	{mod, {www_jabber_se_app, []}},
	{env, [
		{platform, yaws}, %% {inets|yaws|mochiweb}
		{port, 8000},
		{session_timeout, 20},
		{sign_key, "SIGN_KEY"},
		{www_root, "./wwwroot"}
	]}
]}.
