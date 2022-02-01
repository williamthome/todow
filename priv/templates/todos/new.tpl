{% extends "_html.tpl" %}

{% block body %}
  <form method="post" action="{{ form_url }}">
    <input type="text" name="title">
  </form>
{% endblock %}
