#version 420

in vec4 v_texCoord;

uniform sampler2D ssao;

void main()
{
  vec2 texCoord = (v_texCoord.xy / v_texCoord.z + 1) * 0.5;
  //texCoord = gl_FragCoord.xy;
  float occlusion = texture(ssao, texCoord).r;
  vec3 diffuse = vec3(1, 0.5, 0.25);
  gl_FragColor = vec4(diffuse * occlusion, 1);
}