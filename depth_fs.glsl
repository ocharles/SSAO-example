varying vec3 v_position;

void main() {
  gl_FragColor = gl_FragCoord.z / gl_FragCoord.w * 0.5 + 0.5;
}