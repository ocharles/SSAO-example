#version 330

uniform sampler2D uInputTex;

uniform int uBlurSize = 4; // use size of noise texture
uniform vec2 basis;

in vec2 v_texCoord;

out vec3 fResult;

void main() {
   vec2 texelSize = 1.0 / vec2(textureSize(uInputTex, 0));
   vec3 result = vec3(0);
   vec2 hlim = vec2(float(-uBlurSize) * 0.5 + 0.5);
   for (int i = 0; i < uBlurSize; ++i) {
     vec2 offset = (hlim + vec2(float(i)) * basis) * texelSize;
     result += texture(uInputTex, v_texCoord + offset).rgb;
   }

   fResult = result / float(uBlurSize);
}
