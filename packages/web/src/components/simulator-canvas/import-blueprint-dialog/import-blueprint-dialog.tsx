import * as React from "react";
import { useDialog } from "../../dialog";
import {
	type PreviewNode,
	BlueprintPreviewTree,
} from "./blueprint-preview-tree";
import { blueprintStringToPreviewNode } from "./utils";

export type ImportBlueprintDialogProps = {
	onImport: (blueprintText: string) => void;
};

export const ImportBlueprintDialog: React.FC<ImportBlueprintDialogProps> = ({
	onImport,
}) => {
	const { closeDialog } = useDialog();

	const blueprintStringRef = React.useRef<string>("");

	const [preview, setPreview] = React.useState<PreviewNode | null>(null);
	const [error, setError] = React.useState<string | null>(null);

	const handlePreview = React.useCallback((blueprintString: string) => {
		blueprintStringRef.current = blueprintString;

		try {
			const previewNode = blueprintStringToPreviewNode(blueprintString);
			setPreview(previewNode);
			setError(null);
		} catch {
			setPreview(null);
			setError("Invalid blueprint format");
		}
	}, []);

	const handleImport = React.useCallback(() => {
		onImport(blueprintStringRef.current);
	}, [onImport]);

	return (
		<div className="p-3">
			<h2 className="mb-3 text-lg font-semibold text-white">
				Import Blueprint
			</h2>

			<textarea
				// value={text}
				onChange={(e) => handlePreview(e.target.value)}
				placeholder="Paste blueprint JSON here"
				rows={6}
				className="w-full rounded border border-gray-600 bg-neutral-800 px-3 py-2 text-sm text-white outline-none focus:border-blue-500"
			/>

			<div className="mt-3 rounded border border-gray-700 bg-neutral-900 p-2">
				{error && <div className="text-sm text-red-400">{error}</div>}

				{!error && !preview && (
					<div className="text-sm text-gray-500">No preview available</div>
				)}

				{preview && <BlueprintPreviewTree node={preview} />}
			</div>

			<div className="mt-4 flex justify-end gap-2">
				<button
					onClick={closeDialog}
					className="cursor-pointer rounded px-3 py-1.5 text-sm text-gray-300 hover:bg-gray-700"
				>
					Cancel
				</button>

				<button
					onClick={handleImport}
					disabled={!preview}
					className="cursor-pointer rounded bg-blue-600 px-3 py-1.5 text-sm text-white hover:bg-blue-500 disabled:opacity-30 disabled:cursor-not-allowed"
				>
					Import
				</button>
			</div>
		</div>
	);
};
