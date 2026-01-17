import type * as React from "react";
import { config } from "../config";

export const Footer: React.FC = () => {
	return (
		<div className="pointer-events-none fixed bottom-0 right-0 flex justify-center p-1">
			<div className="pointer-events-auto rounded bg-black/40 px-3 py-1 text-xs text-gray-300 backdrop-blur">
				<a
					href={config.githubRepoUrl}
					target="_blank"
					rel="noreferrer"
					className="hover:text-white transition-colors"
				>
					GitHub
				</a>
			</div>
		</div>
	);
};
