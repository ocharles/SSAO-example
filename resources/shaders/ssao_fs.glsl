#version 330

in vec4 v_depthCoord;
in vec3 v_normal;
in vec3 v_position;

uniform mat4 u_proj;
uniform sampler2D u_shadowMap;
uniform vec4 kernel[16];
uniform sampler2D rotations;

const int KERNEL_SIZE = 16;

uniform vec4 uRadius;

void main() {
  vec2 texCoord = v_depthCoord.xy;

  vec3 origin = v_position.xyz;

  vec3 normal = v_normal;
  normal = normalize(normal);

  vec3 rvec = texture(rotations, texCoord * vec2(1024 / 4)).rgb;
  vec3 tangent = normalize(rvec - normal * dot(rvec, normal));
  vec3 bitangent = cross(normal, tangent);
  mat3 tbn = mat3(tangent, bitangent, normal);

  float occlusion = 0.0;
  float sampleDepth;
  vec4 offset;

  float depthHere = gl_FragCoord.z;

  for (int i = 0; i < KERNEL_SIZE; i++) {
    float r = 0.5f;
    vec3 sampleRay = tbn * kernel[int(mod(i,16))].xyz;
    sampleRay = origin + sampleRay * r;

    offset = vec4(sampleRay.xyz, 1.0);
    offset = u_proj * offset;
    offset.xy /= offset.w;
    offset.xy = offset.xy * 0.5 + 0.5;

    sampleDepth = texture2D(u_shadowMap, offset.xy).r;

    float rangeCheck = abs(depthHere - sampleDepth) < r ? 1.0 : 0.0;
    occlusion += (sampleDepth <= depthHere ? 1.0 : 0.0) * rangeCheck;
  }

  occlusion = 1.0f - (occlusion / float(KERNEL_SIZE));

  gl_FragColor = vec4(vec3(occlusion), 1.0f);
}