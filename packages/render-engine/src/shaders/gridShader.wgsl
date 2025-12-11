struct Fragment {
    @builtin(position) Position : vec4 < f32>,
    @location(0) PosNear : vec3f,
    @location(1) PosFar : vec3f
};

struct VSInput{
    @builtin(vertex_index) vertID : u32,
};


@binding(0) @group(0) var<uniform> viewProj : array<mat4x4f, 2>;

fn getUnprojected(point : vec3f, viewProjInv : mat4x4f) -> vec3f{
    var unprojectedPoint = viewProjInv * vec4(point, 1.0);
    return unprojectedPoint.xyz / unprojectedPoint.w;

}

@vertex
fn vs_main(input : VSInput) -> Fragment {
    var pos = array<vec2f, 6 > (
    vec2f(-1, 1),
    vec2f(-1, -1),
    vec2f(1, -1),
    vec2f(-1, 1),
    vec2f(1, -1),
    vec2f(1, 1)
    );
    var output : Fragment;

    output.Position = vec4(pos[input.vertID], 0, 1.0);
    output.PosNear = getUnprojected(vec3(pos[input.vertID], 0), viewProj[1]);
    output.PosFar = getUnprojected(vec3(pos[input.vertID], 1), viewProj[1]);

    return output;
}


struct FSInput{
    @location(0) PosNear : vec3f,
    @location(1) PosFar : vec3f
};
fn gridAA(fragPos : vec2f, scale : f32, lineThickness : f32) -> vec4f{
    const divisions = 8;
    var coord = fragPos * scale * divisions;
    var dist = abs(fract(coord));
    var norm = fwidth(coord);
    norm = clamp(norm, vec2f(0.0, 0.0), vec2f(lineThickness / 2, lineThickness / 2));
    //var norm = vec2f(abs(dpdx(coord.y)), abs(dpdy(coord.x)));
    //var deriv = vec2f(fwidth(coord.x), fwidth(coord.y));
    //var drawWidth = vec2f(clamp(lineThickness, norm.x, 0.5), clamp(lineThickness, norm.y, 0.5));
    norm *= 2.5;
    var grid = smoothstep(lineThickness - norm, lineThickness + norm, dist);

    //var lineWidth = grid.x * grid.y;
    //var lineWidth = mix(grid.x, lineThickness, );
    var grid2 = 1.0 - clamp(grid, vec2f(0.0, 0.0), vec2f(1.0, 1.0));
    //grid2 *= saturate(lineThickness / drawWidth);
    grid2 = mix(grid2, vec2f(lineThickness, lineThickness), saturate(2.0 * norm - 1.0));
    var alpha = mix(grid2.x, 1.0, grid2.y);
    if(alpha < 0.0001)
    {
        return vec4f(0.0, 0.0, 0.0, 0.0);
    }
    return vec4f(0.1 * alpha, 0.1 * alpha, 0.1 * alpha, alpha);

}

@fragment
fn fs_main(input : FSInput) -> @location(0) vec4 < f32> {
    var zScale = -input.PosNear.z / (input.PosFar.z - input.PosNear.z);
    var worldPos = input.PosNear.xy + zScale * (input.PosFar.xy - input.PosNear.xy);
    var screenPos = viewProj[0]*vec4(worldPos.xy, 0, 1.0);
    var zoomLevel = abs(screenPos.z) / 4;
    var zoom = pow(2, floor(zoomLevel));
    //r lineThickness1 = mix(0.005, 0.01, fract(zoomLevel));
    //r lineThickness2 = mix(0.01, 0.005, fract(zoomLevel));
    //r out = mix(gridAA(worldPos, 1 / zoom, lineThickness2), gridAA(worldPos, 1 / (2 * zoom), lineThickness1), fract(zoomLevel));
    var out = gridAA(worldPos, 1 / zoom, 0.02);
    return out;
}
