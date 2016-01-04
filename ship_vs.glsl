#version 330

in vec3 a_position;
in vec3 a_normal;
in vec2 a_uv;

out vec4 v_texCoord;
out vec2 v_uv;
out vec3 v_normal;

uniform mat4 u_view;
uniform mat4 u_model;
uniform mat4 u_proj;

void main() {
    gl_Position = u_proj * u_view * u_model * vec4(a_position, 1);
    v_texCoord = gl_Position;
    v_uv = a_uv;
    v_normal = a_normal;
}
