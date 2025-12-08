
struct Fragment {
    @builtin(position) Position : vec4 < f32>,
    @location(0) Color : vec4 < f32>
};

struct VSInput{
    @builtin(instance_index) instanceID : u32,
    @builtin(vertex_index) vertexID : u32
};

struct UBO{
    model : mat4x4f,
    color : vec3f,
    alpha : f32
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

    output.Position = viewProj[0]*uniforms[input.instanceID].model * vec4 < f32 > (pos[input.vertexID], 0.0, 1.0);
    output.Color = vec4 < f32 > (uniforms[input.instanceID].color, uniforms[input.instanceID].alpha);

    return output;
}

@fragment
fn fs_main(@location(0) Color : vec4 < f32>) -> @location(0) vec4 < f32> {
    return Color;
}
