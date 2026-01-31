import type * as React from "react";
import { config } from "../config";
import { GitHubIcon } from "../icons/github";

export const Footer: React.FC = () => {
	return (
		<div className="fixed bottom-0 right-0 flex justify-center p-1 pointer-events-none">
			<div className="flex items-center gap-2 px-2 py-1 text-xs text-gray-300 rounded pointer-events-auto bg-black/40 backdrop-blur">
				<a
					href={config.githubRepoUrl}
					target="_blank"
					rel="noreferrer"
					className="flex items-center p-1 transition rounded hover:bg-white/10"
					aria-label="Open GitHub repository"
				>
					<GitHubIcon />
				</a>

				<span className="ml-1 text-[10px] text-white/40">v{APP_VERSION}</span>
			</div>
		</div>
	);
};
