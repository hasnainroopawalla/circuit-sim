struct Fragment {
    @builtin(position) Position : vec4 < f32>,
    @location(0) Color : vec4 < f32>,
    @location(1) Uv : vec2 <f32>,
    @location(2) @interpolate(flat) Circle : u32
};

struct FSInput {
    @location(0) Color : vec4 < f32>,
    @location(1) Uv : vec2 <f32>,
    @location(2) @interpolate(flat) Circle : u32
};

struct VSInput{
    @builtin(instance_index) instanceID : u32,
    @builtin(vertex_index) vertexID : u32
};

struct UBO{
    model : mat4x4f,
    color : vec3f,
    alpha : f32,
    radius : vec4f,
};

@binding(0) @group(0) var<uniform> viewProj : array<mat4x4f, 2>;
@binding(0) @group(1) var<storage, read> uniforms : array<UBO>;

@vertex
fn vs_main(input : VSInput) -> Fragment {
    const pos = array(
    vec2(1.0, 1.0),
    vec2(1.0, -1.0),
    vec2(-1.0, -1.0),
    vec2(1.0, 1.0),
    vec2(-1.0, -1.0),
    vec2(-1.0, 1.0),
    );

    var output : Fragment;
    var vertPos = pos[input.vertexID];
    output.Position = viewProj[0]*uniforms[input.instanceID].model * vec4 < f32 > (vertPos, 0.0, 1.0);
    output.Color = vec4 < f32 > (uniforms[input.instanceID].color, uniforms[input.instanceID].alpha);
    output.Uv = vertPos;
    output.Circle=0;
    if(uniforms[input.instanceID].radius.x>0.0){
        output.Circle =1;
    }

    return output;
}

fn fragmentAA(uvPos: vec2f,dist: f32, radius: f32) -> f32{
    var normVec = (fwidth(uvPos));
    var norm = max(normVec.x, normVec.y); 
    var circle = smoothstep(radius-norm,radius+norm,dist);
    var alpha = clamp(circle, 0.0, 1.0);
    alpha = 1-alpha;
    if(alpha<0.001){
        alpha=0.0;
    }
    return alpha;
}

@fragment
fn fs_main(fragInput : FSInput) -> @location(0) vec4 < f32> {
    var color = fragInput.Color;
    var dist = pow(fragInput.Uv.x,2)+pow(fragInput.Uv.y,2);
    var radius=1.0;
    if(fragInput.Circle!=1){
        radius=1.6;
    }
    var alpha = fragmentAA(fragInput.Uv,dist,radius);
    if(alpha<0.01){
        discard;
    }
    return vec4f(color.r*alpha, color.g*alpha, color.b*alpha, alpha); 
}
