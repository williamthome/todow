{% extends "pages/_html.tpl" %}

{% block body %}
  All your tasks will be here o/

  {% button text="Hello" action={postback postback=`world` delegate=`todow`}  %}
{% endblock %}
