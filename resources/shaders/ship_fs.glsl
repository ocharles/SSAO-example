#version 330

in vec4 v_texCoord;
in vec2 v_uv;
in vec3 v_normal;

uniform sampler2D ssao;
uniform sampler2D diffuseMap;

void main()
{
  // TODO Hardcoding scaling factor. We really should be able to calculate this in the vertex shader
  vec2 texCoord = gl_FragCoord.xy * vec2(1.0f/1024.0f);
  float occlusion = texture(ssao, texCoord).r;
  vec3 diffuse = texture(diffuseMap, v_uv).rgb;
  gl_FragColor = vec4(diffuse * occlusion, 1);
}
