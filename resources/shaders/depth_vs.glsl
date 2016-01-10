#version 330

in vec3 a_position;

out vec3 v_position;

uniform mat4 u_projViewModel;

void main() {
    gl_Position = u_projViewModel * vec4(a_position, 1);
}