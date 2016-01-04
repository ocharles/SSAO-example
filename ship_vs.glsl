#version 450

in vec3 a_position;

out vec4 v_texCoord;

uniform mat4 u_view;
uniform mat4 u_model;
uniform mat4 u_proj;

void main() {
    gl_Position = u_proj * u_view * u_model * vec4(a_position, 1);
    v_texCoord = gl_Position;
}