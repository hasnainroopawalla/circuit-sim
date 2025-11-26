struct VSInput{
    @location(0) positions : vec2f
};

struct Fragment{
    @builtin(position) position : vec4f <f32>,
    //@location(0) colour : vec4f <f32>
};

//struct UBO{
//    color : vec3f <f32>,
//    alpha : f32
//};

@binding(0) @group(0) var<uniform> viewProj : array<4x4f, 2>;
//@binding(0) @group(1) var<storage, read> uniforms : array<UBO>;

@vertex
fn vs_main(VSInput input) -> Fragment{
    Fragment output;
    output.position = viewProj[0]*vec4f<f32>(input.positions, 0.0, 1.0);
    return output;
}