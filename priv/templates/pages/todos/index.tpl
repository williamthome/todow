{% extends "pages/_html.tpl" %}

{% block body %}
  All your tasks will be here o/

  {% button text="Hello" action={postback postback=`world` delegate=`todow`}  %}

  {% wire
    type={mqtt topic="bridge/origin/public/hello"}
    action={script script=["console.log('Hello, World!')"]}
  %}
{% endblock %}
