<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Todow</title>

  {% include "_html_head_cotonic.tpl" %}
</head>
<body>
  {% block body %}{% endblock %}

  {% block _js_include %}
    {% include "_js_include_jquery.tpl" %}

    {% lib
      "cotonic/cotonic.js"
      "js/apps/zotonic-wired.js"
    %}

    {% worker
      name="auth"
      src="js/zotonic.auth.worker.js"
      args=%{auth: m.authentication.status}
    %}

    {% script %}
  {% endblock%}

</body>
</html>
