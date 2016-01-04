#version 330

in vec4 v_texCoord;
in vec2 v_uv;

uniform sampler2D ssao;
uniform sampler2D diffuseMap;

void main()
{
  vec2 texCoord = (v_texCoord.xy / v_texCoord.z + 1) * 0.5;
  //texCoord = gl_FragCoord.xy;
  float occlusion = texture(ssao, texCoord).r;
  vec3 diffuse = texture(diffuseMap, v_uv).rgb;
  gl_FragColor = vec4(diffuse * occlusion, 1);
}