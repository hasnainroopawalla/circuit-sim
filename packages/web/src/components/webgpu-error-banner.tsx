import type * as React from "react";

export const WebGpuErrorBanner: React.FC = () => {
	return (
		<div className="flex h-full w-full items-center justify-center bg-neutral-900">
			<div className="max-w-md rounded-lg border border-neutral-700 bg-neutral-800 p-6 text-white">
				<h2 className="mb-2 text-lg font-semibold">WebGPU is not available</h2>

				<p className="mb-4 text-sm text-gray-300">
					This simulator requires WebGPU to render and run simulations. Your
					browser either doesn't support it or it's currently disabled.
				</p>

				<div className="mb-4 text-sm text-gray-400">
					<p className="mb-1 font-medium text-gray-300">
						How to enable WebGPU:
					</p>
					<ul className="list-disc space-y-1 pl-5">
						<li>
							<strong>Chrome / Edge:</strong> Enable WebGPU in{" "}
							<code className="rounded bg-black/30 px-1">chrome://flags</code>
						</li>
						<li>
							<strong>Firefox:</strong> Set{" "}
							<code className="rounded bg-black/30 px-1">
								dom.webgpu.enabled
							</code>{" "}
							to true in <code>about:config</code>
						</li>
						<li>
							<strong>Safari:</strong> Use Safari Technology Preview
						</li>
					</ul>
				</div>
			</div>
		</div>
	);
};
