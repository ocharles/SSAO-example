#version 330

in vec3 a_position;
in vec3 a_normal;

out vec3 v_position;
out vec4 v_depthCoord;
out vec3 v_normal;

uniform mat4 u_view;
uniform mat4 u_model;
uniform mat4 u_proj;

void main()
{
	gl_Position = u_proj * u_view * u_model * vec4(a_position, 1.0);
	v_depthCoord = gl_Position * 0.5f + 0.5f;
  v_normal = a_normal;
  v_position = (u_view * u_model * vec4(a_position, 1.0)).xyz;
}