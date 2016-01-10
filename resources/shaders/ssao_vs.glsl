#version 330

in vec3 a_position;
in vec3 a_normal;

out vec3 v_position;
out vec4 v_depthCoord;
out vec3 v_normal;

uniform mat4 u_projViewModel;
uniform mat4 u_viewModel;

void main()
{
	gl_Position = u_projViewModel * vec4(a_position, 1.0);
	v_depthCoord = gl_Position * 0.5f + 0.5f;
  v_normal = (u_viewModel * vec4(a_normal, 0)).xyz;
  v_position = (u_viewModel * vec4(a_position, 1.0)).xyz;
}