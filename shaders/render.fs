#ifdef NOTHING
#version 300 es
#endif
//
// 2023 @nimadez (https://github.com/nimadez)
//
precision highp float;
precision highp int;
precision highp sampler2D;
precision highp samplerCube;

uniform vec2 uResolution;
uniform int uFrame;
uniform float uTime; // anims
uniform vec3 uPointer;
uniform mat4 uCamMatrix;
uniform bool uIsRendering;
uniform bool uIsReset;
uniform bool uIsPerformance;
uniform int uScene;
uniform sampler2D uBuffer;
uniform sampler2D uNoise;
uniform samplerCube uCubeMap;
uniform sampler2D uHeightMap;
uniform sampler2D uData;

out vec4 glFragColor;

#define MAXDATATEX 2048
const int MAXSAMPLES = 2048;
const float EPSILON = 0.001;

// -------------------------------------------------------
// Structures

struct Material {
    int type;       // [0] emitter [1] diffuse [2] metallic [3] dielectric
    vec3 color;
    float amount;   // amount of emissive, roughness, metalness, ior...
    float thickness;// object thickness for dielectrics
    float pattern;
};

struct SDF {
    float dist;
    Material material;
};

struct Camera {
    vec3 origin;
    vec3 forward;
    vec3 right;
    vec3 up;
    vec3 target;
    float nearPlane;
    float farPlane;
    float fov;
    float aperture;
};

struct Raycast {
    vec3 origin;
    vec3 dir;
    vec3 pos;
    float len;
    int hit;
    SDF map;
};

Camera cam = Camera(
    vec3(0),vec3(0),vec3(0),vec3(0),vec3(0),
    0.1, 100.0, 1.5, 0.00001
);

// -------------------------------------------------------
// Utils

#define PI 3.14159265359
#define PIH 1.57079632679
#define PI2 6.28318530717

float hash(float s) { return fract(sin(s)*43758.5453123); }

float blueNoise(int maxSample) {
    return fract(texelFetch(uNoise, ivec2(gl_FragCoord.xy)/1%ivec2(256.0), 0).r + float(uFrame%maxSample)*1.61803398874989484820459);//Φ golden ratio
}

vec3 cosineDirection(vec3 nor, float seed) {
    float u = hash(78.233 + seed);
    float v = hash(12.873 + seed);
    float a = 6.2831853*v; // method 3
    float b = 2.0*u-1.0;   // by fizzer
    vec3 dir = vec3(sqrt(1.0-b*b)*vec2(cos(a),sin(a)),b);
    return normalize(nor + dir);
}

vec3 uniformVector(float seed) {
    float a = 3.1415926*hash(56.128 + seed);
    float b = 6.2831853*hash(14.836 + seed);
    return vec3(sin(b)*sin(a), cos(b)*sin(a), cos(a));
}

mat3 rotateX(float theta) { float c = cos(theta); float s = sin(theta); return mat3(vec3(1, 0, 0),vec3(0, c, -s),vec3(0, s, c)); }
mat3 rotateY(float theta) { float c = cos(theta); float s = sin(theta); return mat3(vec3(c, 0, s),vec3(0, 1, 0),vec3(-s, 0, c)); }
mat3 rotateZ(float theta) { float c = cos(theta); float s = sin(theta); return mat3(vec3(c, -s, 0),vec3(s, c, 0),vec3(0, 0, 1)); }
vec3 xform(vec3 p, vec3 c) { return p - c; } // translate
vec3 xform(vec3 p, mat3 t) { return p * t; } // rotate
float opUnion(float d1, float d2) { return min(d1,d2); }
float checker(vec2 p) { return mod(floor(p.x) + floor(p.y), 2.0); }
float checker(vec2 p, float m) { return mod(floor(p.x) + floor(p.y), m); }

float getHeightMap(vec3 p) {
    return (texture(uHeightMap, p.xz * 0.08).r +
            texture(uHeightMap, p.xz * 0.04).g * 2.0 +
            texture(uHeightMap, p.xz * 0.02).b * 3.0);
}

vec4 getDataTexture(float offset, float size) {
    return texelFetch(uData, ivec2(mod(offset, size), floor(offset / size)), 0);
}

// -------------------------------------------------------
// SDF primitives and operators

SDF sdSphere(vec3 p, vec3 c, float r, Material material) {
    return SDF(length(p - c) - r, material);
}

SDF sdBox(vec3 p, vec3 s, Material material) {
    vec3 d = abs(p) - s;
    float dist = min(max(d.x, max(d.y, d.z)), 0.0) + length(max(d, 0.0));
    return SDF(dist, material);
}
SDF sdBox(vec3 p, vec3 c, vec3 s, Material material) {
    vec3 d = abs(p - c) - s;
    float dist = min(max(d.x, max(d.y, d.z)), 0.0) + length(max(d, 0.0));
    return SDF(dist, material);
}
SDF sdBox(vec3 p, vec3 c, vec3 s, float r, Material material) {
    vec3 d = abs(p - c) - s;
    float dist = min(max(d.x, max(d.y, d.z)), 0.0) + length(max(d, 0.0)) - r;
    return SDF(dist, material);
}

SDF sdPlaneY(vec3 p, float y, Material material) {
    return SDF(p.y - y, material); // infinite
}

SDF sdCylinderY(vec3 p, vec3 c, float r, float h, Material material) {
    p = p - c;
    vec2 d = abs(vec2(length(p.xz), p.y)) - vec2(r, h);
    float dist = min(max(d.x, d.y), 0.0) + length(max(d, 0.0));
    return SDF(dist, material);
}

SDF sdTorusY(vec3 p, vec3 c, vec2 t, Material material) {
    p = p - c; // t(inner, thickness)
    return SDF(length(vec2(length(p.xz)-t.x,p.y))-t.y, material);
}

SDF sdTube(vec3 p, vec3 a, vec3 b, float r, Material material) {
    vec3 pa = p - a; vec3 ba = b - a;
    float h = clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0);
    float dist = length(pa - ba * h) - r;
    return SDF(dist, material);
}

SDF opUnion(SDF a, SDF b) {
    if (a.dist < b.dist) return a;
    return b;
}

SDF opSubtract(SDF a, SDF b) {
    if (a.dist > -b.dist) return b;
    a.dist = -a.dist;
    return a;
}

SDF opIntersect(SDF a, SDF b) {
    if (a.dist > b.dist) return a;
    return b;
}

Material opMaterialMixer(SDF a, SDF b, float n) {
    vec3 color = mix(a.material.color, b.material.color, 1.0-n);
    float amount = mix(a.material.amount, b.material.amount, 1.0-n);
    float thickness = mix(a.material.thickness, b.material.thickness, 1.0-n);
    float pattern = mix(a.material.pattern, b.material.pattern, 1.0-n);
    return Material(a.material.type, color, amount, thickness, pattern);
}

SDF opSmoothUnion(SDF a, SDF b, float k) {
    float h = clamp(0.5 + 0.5*(b.dist - a.dist)/k, 0.0, 1.0);
    float dist = mix(b.dist, a.dist, h) - k*h*(1.0-h);
    if (dist == a.dist) {
        a.dist = dist;
        a.material = opMaterialMixer(a, b, 0.50*h/k);
        return a;
    } else {
        b.dist = dist;
        b.material = opMaterialMixer(b, a, 0.50*h/k);
        return b;
    }
}

SDF opSmoothSubtract(SDF a, SDF b, float k) {
    float h = clamp(0.5 - 0.5*(b.dist + a.dist)/k, 0.0, 1.0);
    float dist = mix(b.dist, -a.dist, h) + k*h*(1.0-h);
    if (dist == a.dist) {
        a.dist = dist;
        return a;
    } else {
        b.dist = dist;
        return b;
    }
}

SDF opSmoothIntersect(SDF a, SDF b, float k) {
    float h = clamp(0.5 - 0.5*(b.dist - a.dist)/k, 0.0, 1.0);
    float dist = mix(b.dist, a.dist, h) + k*h*(1.0-h);
    if (dist == a.dist) {
        a.dist = dist;
        return a;
    } else {
        b.dist = dist;
        return b;
    }
}

// -------------------------------------------------------
// SDF Scene

Material matEmtW = Material(0, vec3(1), 6.0,0.0, 0.0);
Material matEmtB = Material(0, vec3(0.0,1.0,1.0), 6.0,0.0, 0.0);
Material matEmtY = Material(0, vec3(1.0,1.0,0.0), 6.0,0.0, 0.0);
Material matEmtWarm = Material(0, vec3(1.0,0.8,0.5), 8.0,0.0, 0.0);
Material matWhite = Material(1, vec3(0.85), 0.0,0.0, 0.0);
Material matGold = Material(2, vec3(0.9,0.6,0.2), 0.9,0.0, 0.0);
Material matMetalBlack = Material(2, vec3(0.3), 0.5,0.0, 0.0);
Material matMetalRough = Material(2, vec3(0.8), 0.8,0.0, 0.0);
Material matMirror = Material(2, vec3(1), 0.0,0.0, 0.0);
Material matGlass = Material(3, vec3(1.0,0.2,0.2), 1.51,2.0, 0.0);
Material matRug = Material(1, vec3(0.5,0.5,0.9), 0.8,0.0, 0.0);

SDF sdSceneA(vec3 p) {
    Material test1 = matGold;
    Material test2 = matMirror;
    Material matFloor = Material(1, vec3(0.2), 0.0,0.0, checker(p.xz*1.0, 1.5));
    SDF world = sdBox(p, vec3(0.0,-0.2,0.0), vec3(5.0,0.2,5.0), 0.1, matFloor);
    world = opUnion(world, sdBox(p, vec3(3.0,10.0,0.0), vec3(0.5,0.01,5.0), matEmtW));
    world = opUnion(world, sdBox(p, vec3(0.0,10.0,0.0), vec3(0.5,0.01,5.0), matEmtW));
    world = opUnion(world, sdBox(p, vec3(-3.0,10.0,0.0), vec3(0.5,0.01,5.0), matEmtW));
    world = opUnion(world, opSmoothSubtract(sdSphere(p, vec3(0.8,4.0,0.8), 3.0, test2),
                                            sdSphere(p, vec3(0.0,3.0,0.0), 3.0, test2), 0.5));
    world = opUnion(world, opSmoothSubtract(sdSphere(p, vec3(1.0,4.0,1.0), 1.5, test1),
                                            sdSphere(p, vec3(0.0,3.0,0.0), 2.2, test1), 0.1));
    return world;
}

SDF sdSceneB(vec3 p) {
    SDF box = sdBox(p, vec3(0.0,5.0,0.0), vec3(5.0), matWhite);
    float s = 0.4; //1.0 <3
    for (int m=0; m<4; m++) {
        vec3 a = mod(p*s, 2.0)-1.0;
        s *= 3.0;
        vec3 r = abs(1.0 - 3.0*abs(a));
        float da = max(r.x,r.y);
        float db = max(r.y,r.z);
        float dc = max(r.z,r.x);
        float d = (min(da,min(db,dc))-1.0)/s;
        if (d > box.dist) box.dist = d;
    }
    return box;
}

SDF sdSceneC(vec3 p, bool isTrap) {
    Material matFloor = Material(1, vec3(1), 0.5,0.0, checker(p.xz*1.0));
    Material matWalls = Material(1, vec3(1), 0.5,0.0, checker(p.xz*2.0));
    SDF world = opUnion(opUnion(sdBox(p, vec3(0), vec3(6.0,0.01,5.0), matFloor),
                                sdBox(p, vec3(0.0,10.0,0.0), vec3(4.0,0.01,5.0), matWhite)),
                                sdBox(p, vec3(0.0,5.0,-5.0), vec3(6.0,5.0,0.01), matWalls));
    world = opUnion(world, sdSphere(p, vec3(0.0,9.2,0.0), 0.13, matMirror));
    world = opUnion(world, sdBox(p, vec3(-5.0,10,0.0), vec3(1.0,0.01,5.0), matEmtB));
    world = opUnion(world, sdBox(p, vec3(5.0,10,0.0), vec3(1.0,0.01,5.0), matEmtY));
    world = opUnion(world, sdSphere(p, vec3(-3.0,3.2,-3.6), 1.0, matGold));
    world = opUnion(world, sdSphere(p, vec3(0.0,3.2,-3.6), 1.0, matWhite));
    world = opUnion(world, sdSphere(p, vec3(3.0,3.2,-3.6), 1.0, matGlass));
    world = opUnion(world, opSubtract(sdBox(p, vec3(0.0,1.1,-3.0), vec3(3.6,0.6,1.0), 0.2, matMetalBlack),
                                      sdBox(p, vec3(0.0,1.1,-3.8), vec3(4.0,1.0,1.0), 0.1, matMetalBlack)));
    world = opUnion(world, sdCylinderY(xform(p, rotateX(PI/2.0)), vec3(0.0,5.0,6.0), 3.0, 0.1, matMirror));
    world = opUnion(world, sdTube(p, vec3(0.0,9.3,0.0), vec3(0.0,10.0,0.0), 0.02, matMetalBlack));
    world = opUnion(world, opSubtract(sdSphere(p, vec3(0.0,8.5,0.0), 0.8, matMetalRough),
                                      sdSphere(p, vec3(0.0,9.0,0.0), 0.5, matMetalRough)));
    SDF rug = sdBox(p, vec3(0.0,0.07,-0.95), vec3(3.0,0.01,1.0), 0.02, matRug);
    rug.dist -= checker(p.xz*30.0)*0.01;
    world = opUnion(world, rug);
    if (isTrap) {
        world = opUnion(world, sdTorusY(xform(p, rotateX(PI/2.0)), vec3(0.0,4.9,6.0), vec2(3.0,0.1), matEmtWarm));
        world = opUnion(world, opUnion(sdBox(p, vec3(6.0,5.0,0.0), vec3(0.01,5.0,5.0), matWalls),
                                       sdBox(p, vec3(-6.0,5.0,0.0), vec3(0.01,5.0,5.0), matWalls)));
    } else {
        world = opUnion(world, sdTorusY(xform(p, rotateX(PI/2.0)), vec3(0.0,4.9,6.0), vec2(3.0,0.1), matMirror));
    }
    return world;
}

SDF sdSceneTest(vec3 p) {
    float d = cam.farPlane; // WARN TODO: for-loop is not supposed to run here
    for (int i=0; i<256; i+=6) {
        vec3 pos = getDataTexture(float(i), 2048.0).xyz;
        d = min(d, length(p-pos)-1.0);
    }
    return SDF(d, matWhite);
}

SDF sdScene(vec3 p) {
    //return sdSceneTest(p);
    if (uScene == 0)      return sdSceneA(p);
    else if (uScene == 1) return sdSceneB(p);
    else if (uScene == 2) return sdSceneC(p, false);
    else if (uScene == 3) return sdSceneC(p, true);
    else return sdSceneA(p);
}

// -------------------------------------------------------
// Raycasting (SDF raymarching)

const vec2 NE = vec2(EPSILON, 0.0);
vec3 calcNormal(vec3 p) {
    float d = sdScene(p).dist;
    return normalize(vec3(
        d - sdScene(p - NE.xyy).dist,
        d - sdScene(p - NE.yxy).dist,
        d - sdScene(p - NE.yyx).dist
    ));
}

const int AO = 4;
float calcAO(vec3 p, vec3 n) {
    float occ = 0.0;
    float sca = 1.0;
    for (int i=0; i<AO; i++) {
        float h = 0.01 + 0.05*float(i)/float(AO);
        float d = sdScene(p + h*n).dist;
        occ += -(d-h)*sca;
        sca *= 0.95;
    }
    return clamp(1.0 - 3.0*occ, 0.5, 1.0);
}

const int RAYMAX = 512;    // 1024 is not expensive, but gain nothing considerable
//const float RAYBIAS = 1.0; // lower bias fix normal artifacts, but super expensive
Raycast raycast(vec3 ro, vec3 rd, float nearPlane, float farPlane) {
    Raycast r;
    r.origin = ro;
    r.dir = rd;
    r.hit = 0;
    r.len = nearPlane;
    for (int i=0; i<RAYMAX; i++) {
        r.pos = r.origin + r.dir * r.len;
        r.map = sdScene(r.pos);
        if (r.map.dist < EPSILON) {
            r.hit = 1;
            break;
        }
        r.len += r.map.dist; // * RAYBIAS;
        if (r.len > farPlane) break;
    }
    return r;
}

// -------------------------------------------------------
// Pathtracing

vec3 getEnvironment(vec3 rd, float k) {
    return k * clamp(texture(uCubeMap, rd).rgb, 0.0, 1.0);
}

float calcFresnel(vec3 refr, vec3 norm, float ior, float cosTheta) {
    ior = (ior - 1.0) / (1.0 + ior);
    float F0 = ior * ior; // schlick approximation
    float cosThetaI = (cosTheta < 0.0) ? -cosTheta : dot(refr, norm);
    return F0 + (1.0 - F0) * pow(1.0 - cosThetaI, 5.0);
}

int BOUNCE = 4; // 1 reveals mask, 3 is not enough, >4 gain nothing considerable
vec4 pathtrace(vec3 ro, vec3 rd, float seed) {
    vec3 acc = vec3(0); // accumulation
    vec3 att = vec3(1); // attenuation
    float depth = 0.0;

    for (int b=0; b<BOUNCE; b++) {
        Raycast ray = raycast(ro, rd, cam.nearPlane, cam.farPlane);
        if (b == 0) depth = ray.len;

        if (ray.hit == 1) {
            float k = ray.map.material.amount * ray.map.material.amount;

            if (ray.map.material.type == 0) { // emissive
                acc += att * (k * ray.map.material.color);
                break;
            }

            float cseed = seed + 63.984*float(b);
            vec3 normal = calcNormal(ray.pos);
            ro = ray.pos + normal * EPSILON;

            if (ray.map.material.type == 1) {       // ideal diffuse
                rd = cosineDirection(normal, cseed);
                att *= dot(rd, normal) * (ray.map.material.color + ray.map.material.pattern);
                if (b == 0) att *= calcAO(ro, normal);
            }
            else if (ray.map.material.type == 2) {  // ideal metallic
                float roughness = ray.map.material.amount * ray.map.material.amount;
                rd = normalize(reflect(ray.dir, normal) + uniformVector(cseed)*roughness);
                att *= ray.map.material.color + ray.map.material.pattern;
            }
            else if (ray.map.material.type == 3) {  // ideal dielectric glass
                float cosThetaI = dot(ray.dir, normal); // >0.0 inside
                float eta = (cosThetaI < 0.0) ? (1.0/ray.map.material.amount) : ray.map.material.amount;
                vec3 norm = (cosThetaI < 0.0) ? normal : -normal;
                vec3 refr = refract(ray.dir, norm, eta);
                float F = calcFresnel(refr, norm, ray.map.material.amount, cosThetaI);
                if (hash(cseed) < F) {
                    rd = reflect(ray.dir, norm);
                } else {
                    ro -= norm * (ray.map.material.thickness + 0.01);
                    rd = refr;
                    att *= ray.map.material.color;
                }
            }
            if (uIsReset || uIsPerformance) { // speed up navigation
                BOUNCE = 3;
                rd *= 10.0; // flush rays
            }
        } else {
            if (b == 0) {
                acc = getEnvironment(rd, 2.0);
                acc *= depth;
            } else {
                acc += att * getEnvironment(rd, 8.0);
                depth = (depth > cam.farPlane) ? 0.0 : float(BOUNCE);
            }
            break;
        }
    }
    acc /= float(BOUNCE);
    return vec4(acc, depth);
}

// -------------------------------------------------------
// Rendering

mat3 adaptPerspectiveCamera() {
    cam.origin  = vec3(uCamMatrix[3][0], uCamMatrix[3][1], uCamMatrix[3][2]);
    cam.forward = vec3(uCamMatrix[2][0], uCamMatrix[2][1], uCamMatrix[2][2]);
    cam.right   = vec3(uCamMatrix[0][0], uCamMatrix[0][1], uCamMatrix[0][2]);
    cam.up      = vec3(uCamMatrix[1][0], uCamMatrix[1][1], uCamMatrix[1][2]);
    cam.target  = normalize(cam.origin - cam.forward); // unused
    return mat3(vec3( cam.right.x,    cam.right.y,    cam.right.z),
                vec3( cam.up.x,       cam.up.y,       cam.up.z),
                vec3(-cam.forward.x, -cam.forward.y, -cam.forward.z));
}

const int WEIGHT = 1;
void main()	{
    float seed = blueNoise(MAXSAMPLES);
    float seed1 = hash(seed+12.345);
    float seed2 = hash(seed+56.789);

    vec2 ofAA = vec2(seed1, seed2) - 0.5;
    vec2 aspect = vec2(uResolution.x/uResolution.y, 1.0);
    vec2 uv = (2.0*(gl_FragCoord.xy+ofAA)/uResolution.xy - 1.0) * aspect;

    mat3 matCam = adaptPerspectiveCamera();
    vec3 ro = cam.origin;
    vec3 rd = normalize(matCam * vec3(uv, cam.fov));

    float camSeed1 = seed1 * PI2;
    float camSeed2 = seed2 * cam.aperture;
    float focalDist = raycast(ro, normalize(rd-ro), cam.nearPlane, cam.farPlane).len;
    vec3 focalPoint = focalDist * rd;
    vec3 randAperture = (cos(camSeed1)*cam.right + sin(camSeed1)*cam.up) * sqrt(camSeed2);
    vec3 rdOF = normalize(focalPoint - randAperture);

    vec4 col = vec4(0);
    if (uIsRendering) {
        float prevWeight = float(uFrame) / float(uFrame + WEIGHT);
        float currWeight = 1.0 - prevWeight;
        col = pathtrace(ro + randAperture, rdOF, seed) * currWeight;
        col += texelFetch(uBuffer, ivec2(gl_FragCoord), 0) * prevWeight;
        if (uIsReset)
            col *= 0.1;
    }
    
    glFragColor = col;
}
