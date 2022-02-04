{% extends "pages/_html.tpl" %}

{% block body %}
  {% comment %} <form method="{{ form_method }}" action="{{ form_url }}"> {% endcomment %}
  {% comment %} <form data-onsubmit-topic="model/mymodel/post/form/todo"> {% endcomment %}
  {% comment %} {% wire id="form" type={mqtt topic="model/mymodel/post/form/todo"} action={growl text="hello"} %} {% endcomment %}
  <form method="{{ form_method }}" action="{{ form_action }}">
  {% comment %} {% wire id="myform" type="submit" postback="some_tag" delegate="controller_todos" %} {% endcomment %}
  {% comment %} <form id="myform" method="post" action="postback"> {% endcomment %}
    <input type="hidden" value="{{ resource_id }}">
    <input type="text" name="title">
  </form>
{% endblock %}
