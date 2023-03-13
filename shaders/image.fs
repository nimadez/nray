#ifdef NOTHING
#version 300 es
#endif
//
// 2023 @nimadez (https://github.com/nimadez)
//
precision highp float;
precision highp int;
precision highp sampler2D;

uniform vec2 uResolution;
uniform sampler2D uBuffer;

out vec4 glFragColor;

vec3 tonemapACES(const vec3 x) { // Narkowicz 2015, ACES Filmic Tone Mapping Curve
    return clamp((x * (2.51 * x + 0.03)) / (x * (2.43 * x + 0.59) + 0.14), 0.0, 1.0);
}

void main()	{
    vec2 uv = gl_FragCoord.xy / uResolution.xy;
    vec4 data = texelFetch(uBuffer, ivec2(gl_FragCoord), 0);

    vec3 col = data.rgb / data.a;
    col = tonemapACES(1.0*col);         // tonemapping
    col = pow(col, vec3(0.8,0.85,0.9)); // grading
    col = pow(col, vec3(1.0/2.2));      // gamma and vignette
    col *= vec3(1.0) * smoothstep(1.8,0.5, length(uv*2.0-1.0))*0.5+0.5;
    
    glFragColor = vec4(col, 1.0);
}
