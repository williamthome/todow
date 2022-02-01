{% extends "_html.tpl" %}

{% block body %}
  <form method="{{ form_method }}" action="{{ form_url }}">
    <input type="hidden" value="{{ resource_id }}">
    <input type="text" name="title">
  </form>
{% endblock %}
