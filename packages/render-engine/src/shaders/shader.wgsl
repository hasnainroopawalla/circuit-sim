struct Fragment {
    @builtin(position) Position : vec4 < f32>,
    @location(0) Color : vec4 < f32>,
    @location(1) Uv : vec2 <f32>,
    @location(2) @interpolation(flat) Circle : u32
};

struct VSInput{
    @builtin(instance_index) instanceID : u32,
    @builtin(vertex_index) vertexID : u32
};

struct UBO{
    model : mat4x4f,
    color : vec3f,
    alpha : f32,
    radius : f32
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

    return output;
}

@fragment
fn fs_main(fragInput : Fragment) -> @location(0) vec4 < f32> {
    var color = vec4(0.0, 0.0, 0.0, 0.0);
    if(fragInput.radius > 0.0)
    {

    }
    return Color;
}
