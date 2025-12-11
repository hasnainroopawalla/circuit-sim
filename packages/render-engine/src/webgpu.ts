const ADAPTER_RETRIES = 5;
const ADAPTER_RETRY_INTERVAL_MS = 300;

// global gpu context
const gpu: {
	adapter: GPUAdapter | null;
	device: GPUDevice | null;
	initPromise: Promise<void> | null;
} = {
	adapter: null,
	device: null,
	initPromise: null,
};

export async function getWebGpu() {
	if (gpu.adapter && gpu.device) {
		return gpu;
	}

	if (!gpu.initPromise) {
		gpu.initPromise = init();
	}

	await gpu.initPromise;

	return gpu;
}

async function init() {
	// retry loop because adapter can temporarily fail during HMR
	for (let attempt = 0; attempt < ADAPTER_RETRIES; attempt++) {
		const adapter = await navigator.gpu.requestAdapter();
		if (adapter) {
			const device = await adapter.requestDevice();
			gpu.adapter = adapter;
			gpu.device = device;

			device.lost.then(() => {
				gpu.adapter = null;
				gpu.device = null;
				gpu.initPromise = null;
			});

			return;
		}
		await new Promise((resolve) =>
			setTimeout(resolve, ADAPTER_RETRY_INTERVAL_MS),
		);
	}

	throw new Error("No appropriate GPUAdapter found after retries");
}

export async function initWebGpu(): Promise<GPUDevice> {
	const { device } = await getWebGpu();

	if (!device) {
		throw new Error("No appropriate GPU Device found");
	}

	return device;
}
